use runtime::{BlockContext, TraverseRt};
pub use repl::Repl;
pub use interpreter::Interpreter;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod interpreter;
pub mod err;
mod repl;
mod util;

pub trait Printable {
    fn repr(&self) -> String;
    fn str(&self) -> String;
}