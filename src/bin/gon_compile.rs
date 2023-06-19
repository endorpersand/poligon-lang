use std::path::PathBuf;
use std::process::ExitCode;
use std::{io, fs};

use clap::Parser;
use inkwell::OptimizationLevel;
use inkwell::context::Context;
use poligon_lang::compiler::Compiler;

#[derive(Parser)]
struct Cli {
    /// When enabled, the Poligon standard library is not packaged when compiling
    /// the Poligon file.
    #[arg(long)]
    no_std: bool,

    /// Does not optimize LLVM.
    #[arg(long = "O0", group="optimization")]
    opt_none: bool,
    /// Optimizes LLVM, but less than O2.
    #[arg(long = "O1", group="optimization")]
    opt_less: bool,
    /// Optimizes LLVM a moderate amount.
    #[arg(long = "O2", group="optimization")]
    opt_default: bool,
    /// Optimizes LLVM aggressively.
    #[arg(long = "O3", group="optimization")]
    opt_aggressive: bool,

    /// The file to compile
    file: PathBuf
}

fn main() -> io::Result<ExitCode> {
    let args = Cli::parse();
    let in_path = args.file;

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

    let optimization = match (args.opt_none, args.opt_less, args.opt_default, args.opt_aggressive) {
        (true, _, _, _) => OptimizationLevel::None,
        (_, true, _, _) => OptimizationLevel::Less,
        (_, _, true, _) => OptimizationLevel::Default,
        (_, _, _, true) => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default
    };

    let mut compiler = if args.no_std {
        Compiler::no_std(&ctx, &in_path)
    } else {
        unwrap_or_exit! { Compiler::new(&ctx, &in_path) }
    };
    compiler.set_optimization(optimization);
    
    let code = fs::read_to_string(&in_path)?;
    let (plir, dtypes) = unwrap_or_exit! { compiler.generate_plir(&code) };
    
    let out_path = compiler.default_output_dir();
    fs::create_dir_all(&out_path)?;
    let stem = compiler.get_module_stem();

    // print PLIR
    fs::write(
        out_path.join(format!("{stem}.plir.gon")), 
        plir.to_string()
    )?;

    unwrap_or_exit! { compiler.load_plir(&plir, dtypes) };

    unwrap_or_exit! { compiler.write_to_disk() };
    unwrap_or_exit! { compiler.to_ll(out_path.join(format!("{stem}.ll"))) };

    let exit = unwrap_or_exit! { unsafe { compiler.jit_run() } };
    
    Ok(exit)
}