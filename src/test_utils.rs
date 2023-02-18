use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::fs;
use std::path::Path;

use crate::compiler::{plir, codegen, Compiler};
use crate::err::{FullGonErr, GonErr};
use crate::interpreter::runtime::{RtContext, self};
use crate::interpreter::runtime::value::Value;
use crate::{lexer, ast, parser};
use crate::lexer::token::{FullToken, Token};
use inkwell::context::Context;

pub mod prelude {
    pub use super::TestLoader;
    pub use super::TestResult;
    pub use super::IoExtract;

    macro_rules! load_tests {
        ($f:literal) => {
            load_tests!(tests, $f);
        };
        ($name:ident, $f:literal) => {
            fn $name() -> &'static $crate::test_utils::TestLoader {
                lazy_static::lazy_static! {
                    static ref TEST: $crate::test_utils::TestLoader = $crate::test_utils::TestLoader::new($f).unwrap();
                }
        
                &TEST
            }
        };
    }
    pub(crate) use load_tests;
}

use inkwell::values::FunctionValue;

pub enum TestErr {
    MissingTestHeader,
    DuplicateTest(String),
    IoErr(std::io::Error),
    LexFailed(String),
    TestFailed(String /* name of test */, String /* the error */),
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
            Self::DuplicateTest(name) => write!(f, "duplicate test {name}"),
            Self::IoErr(err) => write!(f, "{err:?}"),
            Self::LexFailed(err) => write!(f, "lexing of test file failed:\n{err}"),
            Self::TestFailed(test, err) => write!(f, "{test} panicked:\n{err}"),
            Self::UnknownTest(name) => write!(f, "unknown test {name}"),
        }
    }
}

pub type TestResult<T> = Result<T, TestErr>;
type Tokens = Vec<FullToken>;

#[derive(Clone)]
pub struct Test<'t> {
    name: &'t str,
    code: &'t str,
    tokens: Vec<FullToken>
}

impl Test<'_> {
    fn transform_result<T, E: GonErr>(&self, r: Result<T, impl Into<FullGonErr<E>>>) -> TestResult<T> {
        r.map_err(|e| e.into().full_msg(self.code))
         .map_err(|e| TestErr::TestFailed(self.name.to_string(), e))
    }

    pub fn parse(&self) -> TestResult<ast::Program> {
        let r = parser::parse(self.tokens.clone());
        self.transform_result(r)
    }

    pub fn interpret(&self) -> TestResult<Value> {
        let mut ctx = RtContext::new();

        let r = self.parse()?.run_with_ctx(&mut ctx);
        self.transform_result(r)
    }

    pub fn interpret_with_io(&self, hook: runtime::IoHook) -> TestResult<Value> {
        let mut ctx = RtContext::new_with_io(hook);

        let r = self.parse()?.run_with_ctx(&mut ctx);
        self.transform_result(r)
    }

    pub fn codegen(&self) -> TestResult<plir::Program> {
        let ast = self.parse()?;

        let r = codegen::codegen(ast);
        self.transform_result(r)
    }

    fn compile_w_ctx<'ctx>(&self, ctx: &'ctx Context) -> TestResult<(Compiler<'ctx>, FunctionValue<'ctx>)> {
        let plir = self.codegen()?;
    
        let mut compiler = Compiler::from_ctx(ctx);
    
        match compiler.compile(&plir) {
            Ok(f) => Ok((compiler, f)),
            Err(e) => self.transform_result(Err(e)),
        }
    }

    pub fn compile(&self) -> TestResult<()> {
        self.compile_w_ctx(&Context::create()).map(|_| ())
    }

    pub unsafe fn jit_run<T>(&self) -> TestResult<T> {
        let ctx = Context::create();
        let (mut compiler, f) = self.compile_w_ctx(&ctx)?;
        
        let r = compiler.jit_run(f);
        self.transform_result(r)
    }
}

pub struct TestLoader(String, HashMap<String, Tokens>);
impl TestLoader {
    pub fn new(fp: impl AsRef<Path>) -> TestResult<Self> {
        let code = fs::read_to_string(fp.as_ref())?;
        let tokens = lexer::tokenize(&code)
            .map_err(|e| e.full_msg(&code))
            .map_err(TestErr::LexFailed)?;
        
        let mut map = HashMap::new();

        for (header, tokens) in split_tests(tokens)? {
            match map.entry(header) {
                Entry::Occupied(e) => Err(TestErr::DuplicateTest(e.remove_entry().0))?,
                Entry::Vacant(e) => e.insert(tokens),
            };
        }

        Ok(Self(code, map))
    }

    pub fn get(&self, id: &str) -> TestResult<Test> {
        match self.1.get_key_value(id) {
            Some((name, tokens)) => Ok(Test {
                name,
                code: &self.0,
                tokens: tokens.clone(),
            }),
            None => Err(TestErr::UnknownTest(id.to_string())),
        }
    }
}

fn test_name_from_token(ft: &FullToken) -> Option<&str> {
    fn test_name(c: &str) -> Option<&str> {
        c.trim()
         .strip_prefix("! TEST ")
         .map(|c| c.trim())
    }

    if let Token::Comment(c, _) = &ft.tt {
        test_name(c)
    } else {
        None
    }
}

fn split_tests(t: impl IntoIterator<Item=FullToken>) -> TestResult<Vec<(String, Tokens)>> {
    let mut it = t.into_iter().peekable();
    let mut tests = vec![];

    let mut header = match it.next() {
        Some(t) => test_name_from_token(&t)
            .ok_or(TestErr::MissingTestHeader)?
            .to_string(),
        None => return Ok(tests),
    };
    let mut current_test = vec![];

    for token in it {
        match test_name_from_token(&token) {
            Some(next_header) => {
                tests.push((header, current_test));

                header = next_header.to_string();
                current_test = vec![];
            },
            None => current_test.push(token),
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
        let t = tests().get("add")?;

        let mut io = IoExtract::new();

        t.interpret_with_io(io.write_hook())?;
        
        let mut lines = io.lines().map(Result::unwrap);
        assert_eq!(lines.next().unwrap(), "4");
        assert!(lines.next().is_none());
        
        Ok(())
    }
}
