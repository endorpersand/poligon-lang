use std::io::{self, BufRead, Write};

use poligon_lang::*;
use poligon_lang::err::{FullGonErr, GonErr};

// TODO: delete once everything's implemented
fn wrap_err<E: std::fmt::Debug>(r: E) -> FullGonErr<impl GonErr> {
    r.into()
}

fn print(txt: &str) -> io::Result<()> {
    print!("{}", txt);
    io::stdout().flush()
}

// If there's a FullGonErr, then print its msg, and continue the line reading loop.
// Otherwise, unwrap the value and return it.
macro_rules! continue_if_err {
    ($e:expr, $t:expr) => {
        match $e {
            Ok(t) => t,
            Err(e) => {
                eprintln!("{}", wrap_err(e).full_msg($t));
                print("> ")?;
                continue;
            }
        }
    }
}

fn main() -> io::Result<()> {
    let mut ctx = BlockContext::new();
    print("> ")?;

    let mut lexer: Option<Lexer> = None;

    for line in io::stdin().lock().lines() {
        // If lexer exists, then the previous line is continuing onto this line:
        let mut lx = if let Some(mut lx) = lexer.take() {
            let mut input = String::from("\n");
            input.push_str(&line?);

            continue_if_err!(lx.append_input(&input), &lx.string);
            lx
        } else {
            let input = &line?;
            continue_if_err!(Lexer::new(input), input)
        };
        
        let txt = lx.string.clone();
        // Lex and make sure no failure:
        continue_if_err!(lx.partial_lex(), &txt);

        // We can't finish lexing, implying there's an open delimiter:
        if let Err(_) = lx.try_close() {
            print("... ")?;

            lexer = Some(lx);
            continue;
        }

        // if we got here, we should be able to close:
        let tokens = continue_if_err!(lx.close(),                 &txt);
        let tree   = continue_if_err!(parse_repl(tokens),         &txt);
        let result = continue_if_err!(tree.traverse_rt(&mut ctx), &txt);

        println!("{:?}", result);
        print("> ")?;
    }

    Ok(())
}