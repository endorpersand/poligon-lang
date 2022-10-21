use crate::FullGonErr;
use crate::Lexer;
use crate::parser::parse_repl;
use crate::runtime::BlockContext;

/// Read-eval-print loop
/// 
/// This REPL is a command-line utility that can read individual lines ([`process_line`]),
/// execute them, and print out their value.
/// 
/// TODO! code 
/// 
/// [`process_line`]: Repl::process_line
pub struct Repl<'ctx> {
    lexer: Option<Lexer>,
    ctx: BlockContext<'ctx>,
    code: String
}

impl Repl<'_> {
    /// Create a new instance of the REPL.
    pub fn new() -> Self {
        Self { lexer: None, ctx: BlockContext::new(), code: String::new() }
    }

    /// Check if the previous input is awaiting another line
    /// 
    /// If true, this input is awaiting another line.
    /// If false, [`process_line`] will start executing from a new line.
    /// 
    /// [`process_line`]: Repl::process_line
    pub fn line_continues(&self) -> bool {
        self.lexer.is_some()
    }

    /// Execute the line given.
    /// 
    /// If the line has an unclosed delimiter, then the REPL will wait 
    /// until the delimiter is closed to execute the line.
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
                        return;
                    }
                }
            }
        }

        let mut lx = if let Some(mut lx) = self.lexer.take() {
            let mut input = String::from("\n");
            input.push_str(line);

            consume_err! { lx.append_input(&input) };
            lx
        } else {
            consume_err! { Lexer::new(line) }
        };

        // Lex the current data in lexer, and check to make sure there's no syntax errors:
        consume_err! { lx.partial_lex() };

        // We can't finish lexing, implying there's an open delimiter:
        if lx.try_close().is_err() {
            self.lexer = Some(lx);
            return;
        }

        // if we got here, we should be able to close:
        let tokens = consume_err! { lx.close() };
        let tree   = consume_err! { parse_repl(tokens) };
        let result = consume_err! { tree.run_with_ctx(&mut self.ctx) };

        // success!
        self.code.clear();
        println!("{}", result.repr());

    }
}