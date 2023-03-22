use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::fs;
use std::iter::Peekable;
use std::path::Path;

use crate::compiler::{plir, plir_codegen, LLVMCodegen};
use crate::err::{FullGonErr, GonErr};
use crate::interpreter::runtime::{RtContext, self};
use crate::interpreter::runtime::value::Value;
use crate::{lexer, ast, parser};
use crate::lexer::token::{FullToken, Token};
use inkwell::context::Context;

pub mod prelude {
    pub use super::{TestLoader, Test, TestResult, IoExtract, with_compiler};

    macro_rules! load_tests {
        ($f:literal) => {
            load_tests!("", $f);
        };
        ($id:literal, $f:literal) => {
            load_tests!($id, TESTS = $f);
        };
        ($($name:ident = $f:literal)+) => {
            load_tests!("", $($name = $f)+);
        };
        ($id:literal, $($name:ident = $f:literal)+) => {
            $(
                lazy_static::lazy_static! {
                    static ref $name: $crate::test_utils::TestLoader = $crate::test_utils::TestLoader::new($id.trim(), $f).unwrap();
                }
            )+
        };
    }
    pub(crate) use load_tests;
}

use inkwell::values::FunctionValue;

pub enum TestErr {
    MissingTestHeader,
    MalformedHeader,
    DuplicateTest(String),
    IoErr(std::io::Error),
    LexFailed(String),
    TestFailed(String /* name of test */, String /* the error */),
    TestPassed(String /* name of test */),
    UnknownTest(String)
}
impl From<std::io::Error> for TestErr {
    fn from(value: std::io::Error) -> Self {
        TestErr::IoErr(value)
    }
}

impl Debug for TestErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingTestHeader => write!(f, "missing test header"),
            Self::MalformedHeader => write!(f, "malformed header"),
            Self::DuplicateTest(name) => write!(f, "duplicate test {name}"),
            Self::IoErr(err) => write!(f, "{err:?}"),
            Self::LexFailed(err) => write!(f, "lexing of test file failed:\n{err}"),
            Self::TestPassed(test) => write!(f, "{test} passed"),
            Self::TestFailed(test, err) => write!(f, "{test} panicked:\n{err}"),
            Self::UnknownTest(name) => write!(f, "unknown test {name}")
        }
    }
}

pub type TestResult<T> = Result<T, TestErr>;
type Tokens = Vec<FullToken>;

#[derive(Clone)]
pub struct Test<'t> {
    pub header: &'t Header,
    code: &'t str,
    tokens: Vec<FullToken>
}

impl Test<'_> {
    pub fn wrap_test_result<T, E: GonErr>(&self, r: Result<T, impl Into<FullGonErr<E>>>) -> TestResult<T> {
        r.map_err(|e| e.into().full_msg(self.code))
         .map_err(|e| TestErr::TestFailed(self.header.name.to_string(), e))
    }

    pub fn parse(&self) -> TestResult<ast::Program> {
        let r = parser::parse(self.tokens.clone());
        self.wrap_test_result(r)
    }

    fn running(&self, loader_id: &str) -> bool {
        !self.header.ignore.iter().any(|id| id == loader_id)
    }

    #[allow(unused)]
    pub fn interpret(&self) -> TestResult<Value> {
        let mut ctx = RtContext::new();

        let r = self.parse()?.run_with_ctx(&mut ctx);
        self.wrap_test_result(r)
    }

    #[allow(unused)]
    pub fn interpret_with_io(&self, hook: runtime::IoHook) -> TestResult<Value> {
        let mut ctx = RtContext::new_with_io(hook);

        let r = self.parse()?.run_with_ctx(&mut ctx);
        self.wrap_test_result(r)
    }

    pub fn codegen(&self) -> TestResult<plir::Program> {
        let ast = self.parse()?;

        let r = plir_codegen::plir_codegen(ast);
        self.wrap_test_result(r)
    }

    #[allow(unused)]
    fn compile_w_ctx<'ctx>(&self, ctx: &'ctx Context) -> TestResult<(LLVMCodegen<'ctx>, FunctionValue<'ctx>)> {
        let plir = self.codegen()?;
    
        let mut compiler = LLVMCodegen::new(ctx);
    
        match compiler.compile(&plir) {
            Ok(f) => Ok((compiler, f)),
            Err(e) => self.wrap_test_result(Err(e)),
        }
    }

    #[allow(unused)]
    pub fn compile(&self) -> TestResult<()> {
        self.compile_w_ctx(&Context::create()).map(|_| ())
    }

    #[allow(unused)]
    pub unsafe fn jit_run<T>(&self) -> TestResult<T> {
        let ctx = Context::create();
        let (mut compiler, f) = self.compile_w_ctx(&ctx)?;
        
        let r = compiler.jit_run(f);
        self.wrap_test_result(r)
    }
}

pub fn with_compiler<T>(mut f: impl FnMut(&mut LLVMCodegen) -> T) -> T {
    let ctx = Context::create();
    let mut compiler = LLVMCodegen::new(&ctx);
    f(&mut compiler)
}

pub struct TestLoader {
    id: &'static str,
    code: String, 
    tests: HashMap<Header, Tokens>
}
impl TestLoader {
    pub fn new(id: &'static str, fp: impl AsRef<Path>) -> TestResult<Self> {
        let code = fs::read_to_string(fp.as_ref())?;
        let tokens = lexer::tokenize(&code)
            .map_err(|e| e.full_msg(&code))
            .map_err(TestErr::LexFailed)?;
        
        let mut tests = HashMap::new();

        for (header, tokens) in split_tests(tokens)? {
            match tests.entry(header) {
                Entry::Occupied(e) => Err(TestErr::DuplicateTest(e.remove_entry().0.name))?,
                Entry::Vacant(e) => e.insert(tokens),
            };
        }

        Ok(Self { id, code, tests })
    }

    pub fn get(&self, id: &str) -> TestResult<Test> {
        match self.tests.get_key_value(id) {
            Some((header, tokens)) => Ok(Test {
                header,
                code: &self.code,
                tokens: tokens.clone(),
            }),
            None => Err(TestErr::UnknownTest(id.to_string())),
        }
    }

    pub fn pass_all<T, I: FromIterator<T>>(&self, mut f: impl FnMut(Test) -> TestResult<T>, tests: &[&str]) -> TestResult<I> {
        tests.iter()
            .filter_map(|name| {
                match self.get(name) {
                    Ok(test) => test.running(self.id).then(|| f(test)),
                    Err(err) => Some(Err(err)),
                }
            })
            .collect()
    }

    pub fn fail_all<T>(&self, mut f: impl FnMut(Test) -> TestResult<T>, tests: &[&str]) -> TestResult<()> {
        tests.iter()
            .try_for_each(|&t| {
                let test = self.get(t)?;
                if test.running(self.id) {
                    match f(test) {
                        Ok(_) => Err(TestErr::TestPassed(String::from(t))),
                        Err(_) => Ok(()),
                    }
                } else {
                    Ok(())
                }
            })
    }
}

pub struct Header {
    pub name: String,
    ignore: Vec<String>
}
impl Header {
    fn parse(t: &mut Peekable<impl Iterator<Item=FullToken>>) -> TestResult<Option<Header>> {
        let mut name = None;
        let mut ignore = vec![];

        while let Some(FullToken { tt: Token::Comment(c, _), ..}) = t.peek() {
            if !c.trim().starts_with('!') {
                break;
            }

            let Some(FullToken { tt: Token::Comment(c, _), ..}) = t.next() else { unreachable!() };

            let data = c.trim()
                .strip_prefix('!')
                .unwrap()
                .trim();
            
            if let Some(test_name) = data.strip_prefix("TEST") {
                if name.replace(test_name.trim().to_string()).is_some() {
                    return Err(TestErr::MalformedHeader);
                }
            } else if let Some(ignores) = data.strip_prefix("IGNORE") {
                let ignores = ignores.split(',')
                    .map(str::trim)
                    .map(ToString::to_string);
                ignore.extend(ignores);
            }
        }

        if let Some(name) = name {
            Ok(Some(Header { name, ignore }))
        } else if !ignore.is_empty() {
            Err(TestErr::MalformedHeader)
        } else {
            Ok(None)
        }
    }
}
impl PartialEq for Header {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Header {}
impl std::hash::Hash for Header {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
impl Borrow<str> for Header {
    fn borrow(&self) -> &str {
        &self.name
    }
}

fn split_tests(t: impl IntoIterator<Item=FullToken>) -> TestResult<Vec<(Header, Tokens)>> {
    let mut it = t.into_iter().peekable();
    let mut tests = vec![];

    let Some(mut header) = Header::parse(&mut it)? else {
        if it.peek().is_some() {
            return Err(TestErr::MissingTestHeader)
        } else {
            return Ok(tests)
        }
    };

    let mut current_test = vec![];

    while it.peek().is_some() {
        match Header::parse(&mut it)? {
            Some(next_header) => {
                tests.push((header, current_test));
    
                header = next_header;
                current_test = vec![];
            },
            None => current_test.push(it.next().unwrap()),
        }
    }
    
    tests.push((header, current_test));
    Ok(tests)
}

mod exio {
    use std::collections::VecDeque;
    use std::io::{BufReader, prelude::*};

    use crate::interpreter::runtime::IoHook;

    pub struct IoExtract {
        inner: BufReader<VecDeque<u8>>
    }

    impl IoExtract {
        pub fn new() -> Self {
            Self {
                inner: BufReader::new(Default::default())
            }
        }

        pub fn write_hook(&mut self) -> IoHook {
            IoHook::new_w(self.inner.get_mut())
        }
    }

    impl Read for IoExtract {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            self.inner.read(buf)
        }
    }
    impl BufRead for IoExtract {
        fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
            self.inner.fill_buf()
        }

        fn consume(&mut self, amt: usize) {
            self.inner.consume(amt)
        }
    }
    impl Write for IoExtract {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.inner.get_mut().write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.inner.get_mut().flush()
        }
    }
}
pub use exio::IoExtract;


#[cfg(test)]
mod tests {
    use std::io::BufRead;

    use super::prelude::*;

    load_tests!("_test_files/test_utils.gon");

    #[test]
    fn hello() -> TestResult<()> {
        let t = TESTS.get("add")?;

        let mut io = IoExtract::new();

        t.interpret_with_io(io.write_hook())?;
        
        let mut lines = io.lines().map(Result::unwrap);
        assert_eq!(lines.next().unwrap(), "4");
        assert!(lines.next().is_none());
        
        Ok(())
    }
}
