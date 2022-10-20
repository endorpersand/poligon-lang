use std::io::{self, BufRead, Write};

use poligon_lang::*;

fn print_out(txt: &str) -> io::Result<()> {
    print!("{}", txt);
    io::stdout().flush()
}

fn main() -> io::Result<()> {
    let args: Vec<_> = std::env::args().collect();
    let mfp = args.get(1);

    match mfp {
        Some(fp) => {
            let ir = Interpreter::from_file(fp)?;
            
            match ir.run() {
                Ok(_) => (),
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                },
            };

            Ok(())
        },
        None => open_repl(),
    }
}

fn open_repl() -> io::Result<()> {
    print_out("> ")?;
    let mut repl = Repl::new();

    for line in io::stdin().lock().lines() {
        repl.process_line(&line?);
        print_out(if repl.line_continues() { "... " } else { "> " })?;
    }

    Ok(())
}