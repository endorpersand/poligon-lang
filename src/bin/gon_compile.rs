use std::path::PathBuf;
use std::io;

use clap::Parser;
use inkwell::context::Context;
use poligon_lang::compiler::{Compiler, GonSaveTo};

#[derive(Parser)]
struct Cli {
    /// When enabled, the Poligon standard library is not packaged when compiling
    /// the Poligon file.
    #[arg(long)]
    no_std: bool,

    /// The file to compile
    file: PathBuf
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let fp = args.file;

    macro_rules! unwrap_or_exit {
        ($r:expr) => {
            match $r {
                Ok(t) => t,
                Err(cerr) => {
                    eprintln!("{cerr}");
                    std::process::exit(1);
                }
            }
        }
    }

    let ctx = Context::create();
    let name = fp.file_name()
        .expect("Valid file name")
        .to_str()
        .expect("UTF-8");
    let mut compiler = if args.no_std {
        Compiler::no_std(&ctx, name)
    } else {
        unwrap_or_exit! { Compiler::new(&ctx, name) }
    };
    
    unwrap_or_exit! { compiler.load_gon_and_save_plir(&fp, Some(fp.with_extension("plir.gon"))) };

    unwrap_or_exit! { compiler.write_files(GonSaveTo::SameLoc(fp.as_ref())) };
    unwrap_or_exit! { compiler.to_ll(fp.with_extension("ll")) };

    unwrap_or_exit! { unsafe { compiler.jit_run::<()>() } }
    
    Ok(())
}