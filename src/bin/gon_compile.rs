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

    #[arg(short = 'o', long)]
    /// Path to export files to
    out: Option<PathBuf>,
    
    /// Exports an .ll file alongside module files
    #[arg(long)]
    export_ll: bool,
    /// Exports a .plir file alongside module files
    #[arg(long)]
    export_plir: bool,

    /// The file to compile
    file: PathBuf
}

fn main() -> io::Result<ExitCode> {
    let args = Cli::parse();
    let in_path = args.file;
    let out_path = args.out.unwrap_or_else(|| in_path.with_extension(""));

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
    
    
    fs::create_dir_all(&out_path)?;

    if args.export_plir {
        let code = fs::read_to_string(&in_path)?;
        let (plir, plir_exports) = unwrap_or_exit! { compiler.generate_plir(&code) };

        // print PLIR:
        let name = in_path.file_name().unwrap();
        fs::write(
            out_path.join(name).with_extension("plir.gon"), 
            plir.to_string()
        )?;

        unwrap_or_exit! { compiler.load_plir(&plir, plir_exports) };
    } else {
        unwrap_or_exit! { compiler.load_gon_file(&in_path) };
    }

    let exporter = compiler.into_exporter();
    exporter.optimize_module(optimization);

    unwrap_or_exit! { exporter.write(&out_path) };
    if args.export_ll {
        let out = out_path.join(exporter.get_module_name()).with_extension("ll");
        unwrap_or_exit! { exporter.write_ll(out) };
    }

    let exit = unwrap_or_exit! { unsafe { exporter.jit_run() } };
    Ok(exit)
}