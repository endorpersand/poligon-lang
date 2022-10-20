use crate::FullGonErr;
use crate::Lexer;
use crate::parser::parse_repl;
use crate::runtime::BlockContext;

pub struct Repl<'ctx> {
    lexer: Option<Lexer>,
    ctx: BlockContext<'ctx>,
    code: String
}

impl Repl<'_> {
    pub fn new() -> Self {
        Self { lexer: None, ctx: BlockContext::new(), code: String::new() }
    }

    pub fn line_continues(&self) -> bool {
        self.lexer.is_some()
    }

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
        if let Err(_) = lx.try_close() {
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