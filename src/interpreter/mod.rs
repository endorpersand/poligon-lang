//! Converts text into a running program via interpreting.
//! 
//! This differs from compilation because the AST is directly used
//! in runtime to execute the program. The AST needs to be
//! recreated in order to run the program again.
//! 
//! In compilation, this AST is compiled to lower-level executable code,
//! which can be executed as many times as needed.
//! 
//! This module provides:
//! - [`Interpreter`]: A struct which does the full interpreting from string to execution.
//! - [`Repl`]: A struct which performs read-eval-print loop evaluation in the command line 
//! via the interpreter's runtime.
//! - [`semantic`], [`runtime`]: Modules which execute an AST

use std::{io, fs};
use std::path::Path;

use crate::{lexer, parser, FullGonErr, ast};
use runtime::Value;
use runtime::{BlockContext, TraverseRt};
pub use repl::Repl;

pub mod semantic;
pub mod runtime;
mod repl;

/// The struct that performs the interpretation of strings to executable code.
/// 
/// This struct creates an AST out of a provided string, and directly runs it 
/// through a traversal sequence and runtime processed through Rust's runtime.
/// 
/// As such, this struct may be more limited than the [compiler][crate::compiler] form.
/// 
/// # Usage
/// 
/// The Interpreter struct is a useful 
/// for performing operations from a string.
/// 
/// It can lex from a string:
/// ```
/// use poligon_lang::Interpreter;
/// use poligon_lang::lexer::token::{Token, token};
/// 
/// let interpreter = Interpreter::from_string("print(0);");
/// assert_eq!(interpreter.lex().unwrap(), vec![
///     Token::Ident(String::from("print")),
///     token!["("],
///     Token::Numeric(String::from("0")),
///     token![")"],
///     token![;]
/// ]);
/// ```
/// 
/// It can parse from a string:
/// ```
/// # use poligon_lang::Interpreter;
/// use poligon_lang::ast::*;
/// 
/// # let interpreter = Interpreter::from_string("print(0);");
/// assert_eq!(interpreter.parse().unwrap(), Program(Block(vec![
///     Stmt::Expr(Expr::Call {
///         funct: Box::new(Expr::Ident(String::from("print"))),
///         params: vec![
///             Expr::Literal(Literal::Int(0))
///         ]
///     })
/// ])));
/// ```
/// 
/// And it can also execute from a string:
/// ```
/// # use poligon_lang::Interpreter;
/// use poligon_lang::interpreter::runtime::Value;
/// 
/// # let interpreter = Interpreter::from_string("print(0);");
/// // prints 0
/// assert_eq!(interpreter.run().unwrap(), Value::Unit);
/// ```
pub struct Interpreter {
    source: String
}

type InterpretResult<T> = Result<T, String>;

impl Interpreter {
    /// Create an interpreter from a string
    pub fn from_string(s: &str) -> Self {
        Self {
            source: String::from(s)
        }
    }

    /// Read the text from a file and create an interpreter out of it if successfully read
    pub fn from_file(fp: impl AsRef<Path>) -> io::Result<Self> {
        fs::read_to_string(fp).map(|source| Self { source })
    }

    /// Lex the source string.
    pub fn lex(&self) -> InterpretResult<Vec<lexer::token::FullToken>> {
        lexer::tokenize(&self.source)
            .map_err(|err| err.full_msg(&self.source))
    }

    /// Parse the source string.
    pub fn parse(&self) -> InterpretResult<ast::Program> {
        let lexed = self.lex()?;

        parser::parse(lexed)
            .map_err(|err| err.full_msg(&self.source))
    }

    /// Execute the source string.
    pub fn run(&self) -> InterpretResult<Value> {
        let parsed = self.parse()?;
        
        parsed.run()
            .map_err(|err| FullGonErr::from(err).full_msg(&self.source))
    }
}