use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::{io, fs};

use inkwell::context::Context;
use poligon_lang::compiler::{Compiler, GonSaveTo};
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

            let ctx = Context::create();
            let mut compiler = unwrap_or_exit! { Compiler::new(&ctx, fp) };
            
            let plir = unwrap_or_exit! { compiler.load_gon(&code) };
            let path = change_ext(fp, "plir.gon");
            let mut f = File::create(path)?;
            f.write_all(plir.to_string().as_bytes())?;

            unwrap_or_exit! { compiler.write_files(GonSaveTo::SameLoc(fp.as_ref())) };
            unwrap_or_exit! { compiler.to_ll(change_ext(fp, "ll")) };

            unwrap_or_exit! { unsafe { compiler.jit_run::<()>() } }
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