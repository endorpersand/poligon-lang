use crate::FullGonErr;
use crate::lexer::Lexer;
use crate::parser::Parser;
use super::runtime::BlockContext;

/// Read-eval-print loop
/// 
/// This REPL is a command-line utility that can read individual lines ([`process_line`]),
/// execute them, and print out their value.
/// 
/// A very simple REPL implementation (and almost how the REPL is implemented) can be done as so:
/// ```no_run
/// # use poligon_lang::interpreter::Repl;
/// use std::io::{self, stdin, BufRead};
/// 
/// fn main() -> io::Result<()> {
///     let mut repl = Repl::new();
///     
///     for line in stdin().lock().lines() {
///         repl.process_line(&line?);
///     }
/// 
///     Ok(())
/// }
/// ```
/// 
/// [`process_line`]: Repl::process_line
pub struct Repl<'ctx> {
    lexer: Option<Lexer>,
    ctx: BlockContext<'ctx>,
    code: String,

    succ_last: bool
}

impl Repl<'_> {
    /// Create a new instance of the REPL.
    /// 
    /// # Example
    /// 
    /// ```
    /// # use poligon_lang::interpreter::Repl;
    /// #
    /// let mut repl = Repl::new();
    /// repl.process_line("1 + 2"); // prints 3
    /// repl.process_line("2 + 3"); // prints 5
    /// repl.process_line("3 + 4"); // prints 7
    /// ```
    pub fn new() -> Self {
        Self { 
            lexer: None, 
            ctx: BlockContext::new(), 
            code: String::new(), 
            succ_last: true
        }
    }

    /// Test if the current input spans more than one line.
    /// 
    /// # Example
    /// 
    /// ```
    /// # use poligon_lang::interpreter::Repl;
    /// #
    /// let mut repl = Repl::new();
    /// repl.process_line("1 + 2");
    /// assert!(!repl.line_continues());
    /// 
    /// repl.process_line("[");
    /// assert!(repl.line_continues());
    /// repl.process_line("1, 2, 3, 4");
    /// assert!(repl.line_continues());
    /// repl.process_line("]");
    /// assert!(!repl.line_continues());
    /// ```
    pub fn line_continues(&self) -> bool {
        self.lexer.is_some()
    }

    /// Execute the line given.
    /// 
    /// Typically, this will parse the line in full and execute it.
    /// However, in situations with unclosed delimiters, the REPL may wait for more lines
    /// so that the delimiter can be closed.
    /// 
    /// # Example
    /// 
    /// ```
    /// # use poligon_lang::interpreter::Repl;
    /// #
    /// let mut repl = Repl::new();
    /// 
    /// repl.process_line("1 + 2 + 3"); // returns 6
    /// 
    /// repl.process_line("let a = 1"); // returns void
    /// 
    /// repl.process_line("[");
    /// repl.process_line("a, a, a, a");
    /// repl.process_line("]"); // returns [1, 1, 1, 1]
    /// ```
    pub fn process_line(&mut self, line: &str) {
        // If lexer exists, then the previous line is continues onto this line:

        // add code to stored repl:
        self.code.push_str(line);
        self.code.push('\n');

        /// If there's a FullGonErr, then print its msg, and stop processing this line.
        /// 
        /// Otherwise, unwrap the value and provide the value.
        macro_rules! consume_err {
            ($e:expr) => {
                match $e {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{}", FullGonErr::from(e).full_msg(&self.code));
                        self.code.clear();
                        self.succ_last = false;
                        return;
                    }
                }
            }
        }

        let mut lx = if let Some(mut lx) = self.lexer.take() {
            let mut input = String::from("\n");
            input.push_str(line);

            lx.append(&input);
            lx
        } else {
            Lexer::new(line, true)
        };

        // Lex the current data in lexer, and check to make sure there's no syntax errors:
        consume_err! { lx.lex() };

        // We can't finish lexing, implying there's an open delimiter:
        if lx.try_close().is_err() {
            self.lexer = Some(lx);
            return;
        }

        // if we got here, we should be able to close:
        let tokens = consume_err! { lx.close() };
        let tree   = consume_err! { Parser::new(tokens, true).parse() };
        let result = consume_err! { tree.run_with_ctx(&mut self.ctx) };

        // success!
        self.code.clear();
        self.succ_last = true;
        println!("{}", result.repr());
    }

    /// Test if the last executed line was successful.
    /// 
    /// # Example
    /// ```
    /// # use poligon_lang::interpreter::Repl;
    /// #
    /// let mut repl = Repl::new();
    /// 
    /// repl.process_line("1 + 2 + 3"); // returns 6
    /// assert!(repl.exec_successful());
    /// 
    /// repl.process_line("1 + 'q'"); // type error
    /// assert!(!repl.exec_successful());
    /// ```
    pub fn exec_successful(&self) -> bool {
        self.succ_last
    }
}

impl Default for Repl<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::repl::*;

    #[test]
    fn basic_repl() {
        let mut repl = Repl::new();

        // normal
        repl.process_line("2 + 2");
        assert!(repl.exec_successful());
        repl.process_line("2 + 3");
        assert!(repl.exec_successful());

        // errors to eprintln
        repl.process_line("(;");
        assert!(!repl.exec_successful());
        repl.process_line("2 % 0");
        assert!(!repl.exec_successful());

        // can proceed as normal
        repl.process_line("2 + 4");
        assert!(repl.exec_successful());
    }
    #[test]
    fn multiline_sq_bracket() {
        let mut repl = Repl::new();

        repl.process_line("[1, 2, 3, 4]");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());
        
        repl.process_line("[");
        assert!(repl.line_continues());
        repl.process_line("1,");
        assert!(repl.line_continues());
        repl.process_line("2,");
        assert!(repl.line_continues());
        repl.process_line("3,");
        assert!(repl.line_continues());
        repl.process_line("4");
        assert!(repl.line_continues());
        repl.process_line("]");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());
    }

    #[test]
    fn multiline_paren() {
        let mut repl = Repl::new();

        repl.process_line("(1 + 2)");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());

        repl.process_line("(");
        assert!(repl.line_continues());
        repl.process_line("1 + 2");
        assert!(repl.line_continues());
        repl.process_line(")");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());

        repl.process_line("(");
        assert!(repl.line_continues());
        repl.process_line("1");
        assert!(repl.line_continues());
        repl.process_line("+");
        assert!(repl.line_continues());
        repl.process_line("2");
        assert!(repl.line_continues());
        repl.process_line(")");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());
    }

    #[test]
    fn multiline_curly() {
        let mut repl = Repl::new();

        repl.process_line("{print(1); print(2); print(3);}");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());

        repl.process_line("{");
        assert!(repl.line_continues());
        repl.process_line("print(1);");
        assert!(repl.line_continues());
        repl.process_line("print(2);");
        assert!(repl.line_continues());
        repl.process_line("print(3);");
        assert!(repl.line_continues());
        repl.process_line("}");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());
    }

    #[test]
    fn multiline_string() {
        let mut repl = Repl::new();

        // "
        // a
        // b
        // c
        // "

        repl.process_line("\"");
        assert!(repl.line_continues());
        repl.process_line("a");
        assert!(repl.line_continues());
        repl.process_line("b");
        assert!(repl.line_continues());
        repl.process_line("c");
        assert!(repl.line_continues());
        repl.process_line("\"");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());
    }

    #[test]
    fn multiline_comment() {
        let mut repl = Repl::new();

        repl.process_line("/* /* /* */ */ */");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());

        repl.process_line("/*");
        assert!(repl.line_continues());
        repl.process_line("/*");
        assert!(repl.line_continues());
        repl.process_line("/*");
        assert!(repl.line_continues());
        repl.process_line("*/");
        assert!(repl.line_continues());
        repl.process_line("*/");
        assert!(repl.line_continues());
        repl.process_line("*/");
        assert!(!repl.line_continues());
        assert!(repl.exec_successful());
        repl.process_line("*/");
        assert!(!repl.exec_successful());
    }
}