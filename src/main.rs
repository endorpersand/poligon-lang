use std::io::{self, BufRead, Write};

use poligon_lang::*;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut ctx = BlockContext::new();
    print!("> ");
    stdout.flush()?;

    let mut lexer: Option<Lexer> = None;

    for line in stdin.lock().lines() {
        let mlx = if let Some(mut lx) = lexer.take() {
            let mut input = String::from("\n");
            input.push_str(&line?);

            lx.append_input(&input).map(|()| lx)
        } else {
            Lexer::new(&line?)
        };
        let mut lx = match mlx {
            Ok(lx) => lx,
            Err(e) => {
                println!("syntax error: {:?}", e);
                continue;
            }
        };
        // Lexing failed:
        if let Err(e) = lx.partial_lex() {
            println!("syntax error: {:?}", e);
            continue;
        }
        // We can't finish lexing, implying there's an open delimiter?
        if let Err(_) = lx.try_close() {
            print!("...");
            stdout.flush()?;

            lexer = Some(lx);
            continue;
        }
        match lx.close() {
            Ok(tokens) => {
                match parse_repl(tokens) {
                    Ok(tree) => match tree.traverse_rt(&mut ctx) {
                        Ok(result) => println!("{:?}", result),
                        Err(e) => println!("runtime error: {:?}", e)
                    }
                    Err(e) => {
                        println!("syntax error: {:?}", e);
                    }
                }
            }
            Err(e) => {
                println!("syntax error: {:?}", e);
            }
        }

        print!("> ");
        stdout.flush()?;
    }

    Ok(())
}