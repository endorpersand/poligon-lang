use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::{io, fs};

use inkwell::context::Context;
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

            let path = change_ext(fp, "plir.gon");
            let mut f = File::create(path)?;
            f.write_all(plir.to_string().as_bytes())?;

            let ctx = Context::create();
            let mut compiler = Compiler::from_ctx(&ctx);

            let fun = unwrap_or_exit! { compiler.compile(&plir) };
            
            compiler.to_ll(change_ext(fp, "ll"))?;
            compiler.to_bc(change_ext(fp, "bc"));

            unwrap_or_exit! { unsafe { compiler.jit_run::<()>(fun) } }
            Ok(())
        },
        None => panic!("Missing file"),
    }
}

fn change_ext(p: impl AsRef<Path>, ext: &str) -> PathBuf {
    let mut path = p.as_ref().to_owned();
    path.set_extension(ext);

    path
}