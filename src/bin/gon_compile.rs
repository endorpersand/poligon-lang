use std::{io, fs};

use inkwell::context::Context;
use inkwell::values::AnyValue;
use poligon_lang::compiler::{codegen, Compiler};
use poligon_lang::{lexer, parser};
use poligon_lang::err::FullGonErr;

fn main() -> io::Result<()> {
    let args: Vec<_> = std::env::args().collect();
    let mfp = args.get(1);
    
    match mfp {
        Some(fp) => {
            let code = fs::read_to_string(fp)?;
            
            macro_rules! unwrap_or_exit {
                ($r:expr) => {
                    match $r {
                        Ok(t) => t,
                        Err(fe) => {
                            eprintln!("{}", FullGonErr::from(fe).full_msg(&code));
                            std::process::exit(1);
                        }
                    }
                }
            }

            let tokens = unwrap_or_exit! { lexer::tokenize(&code) };
            let ast    = unwrap_or_exit! { parser::parse(tokens)  };
            let plir   = unwrap_or_exit! { codegen::codegen(ast)  };

            let ctx = Context::create();
            let mut compiler = Compiler::from_ctx(&ctx);

            let fun = unwrap_or_exit! { compiler.compile(&plir) };
            println!("{}", fun.print_to_string().to_string());
            // let ir = Interpreter::from_file(fp)?;
            
            // match ir.run() {
            //     Ok(_) => (),
            //     Err(e) => {
            //         eprintln!("{}", e);
            //         std::process::exit(1);
            //     },
            // };

            Ok(())
        },
        None => panic!("Missing file"),
    }
}