#![allow(unused)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::fs;
use std::iter::Peekable;
use std::ops::RangeBounds;
use std::path::Path;

use crate::compiler::CompileErr;
use crate::err::{FullGonErr, GonErr};
use crate::span::Spanned;
use crate::{lexer, ast, parser};
use crate::lexer::token::{FullToken, Token, OwnedStream, TokenTree};

pub mod prelude {
    pub use super::{TestLoader, Test, TestResult, TestErr};

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
                static $name: ::once_cell::sync::Lazy<$crate::test_utils::TestLoader> = ::once_cell::sync::Lazy::new(|| {
                    $crate::test_utils::TestLoader::new($id.trim(), $f).unwrap()
                });
            )+
        };
    }
    pub(crate) use load_tests;
}

pub enum TestErr {
    MissingTestHeader,
    MalformedHeader,
    DuplicateTest(String),
    IoErr(std::io::Error),
    LexFailed(String),
    TestFailed(String /* name of test */, String /* the error */),
    TestPassed(String /* name of test */),
    UnknownTest(String),
    ExitWrong(u8)
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
            Self::UnknownTest(name) => write!(f, "unknown test {name}"),
            Self::ExitWrong(code) => write!(f, "program exited with code {code:?}")
        }
    }
}

pub type TestResult<T> = Result<T, TestErr>;

#[derive(Clone)]
pub struct Test<'t> {
    pub header: &'t Header,
    code: &'t str,
    tokens: OwnedStream
}

impl Test<'_> {
    pub fn source(&self) -> &str {
        match self.tokens.first().zip(self.tokens.last()) {
            Some((first, last)) => {
                let span = first.span() + last.span();
                &self.code[span.start() .. span.end()]
            }
            None => panic!("Test has no tokens.")
        }
    }

    pub fn wrap_err<E: GonErr + Into<CompileErr>>(&self, e: FullGonErr<E>) -> TestErr {
        let err = e.map(Into::into);

        match err.err {
            CompileErr::IoErr(e) => TestErr::IoErr(e),
            _ => TestErr::TestFailed(self.header.name.to_string(), err.full_msg(self.code))
        }
    }

    pub fn parse(&self) -> TestResult<ast::Program> {
        parser::parse(&self.tokens)
            .map_err(|e| self.wrap_err(e))
    }

    fn running(&self, loader_id: &str) -> bool {
        !self.header.ignore.iter().any(|id| id == loader_id)
    }
}
pub struct TestLoader {
    id: &'static str,
    code: String, 
    tests: HashMap<Header, OwnedStream>
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
    fn parse(t: &mut Peekable<impl Iterator<Item=TokenTree>>) -> TestResult<Option<Header>> {
        let mut name = None;
        let mut ignore = vec![];

        while let Some(TokenTree::Token(FullToken { kind: Token::Comment(c, _), ..})) = t.peek() {
            if !c.trim().starts_with('!') {
                break;
            }

            let Some(TokenTree::Token(FullToken { kind: Token::Comment(c, _), ..})) = t.next() else { unreachable!() };

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

fn split_tests(t: impl IntoIterator<Item=TokenTree>) -> TestResult<Vec<(Header, OwnedStream)>> {
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

#[cfg(test)]
mod tests {
    use super::prelude::*;

    load_tests!("_test_files/test_utils.gon");
}
