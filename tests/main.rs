use std::path::Path;
use std::process::ExitCode;

use inkwell::context::Context;
use poligon_lang::ast::Program;
use poligon_lang::compiler::Compiler;
use poligon_lang::lexer::tokenize;
use poligon_lang::parser::parse;

fn compile_and_run(fp: impl AsRef<Path>) -> ExitCode {
    let ctx = Context::create();

    let path = fp.as_ref();
    let name = path.file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let mut compiler = Compiler::new(&ctx, name).unwrap();
    compiler.load_gon_file(path).unwrap();
    
    let exporter = compiler.into_exporter();
    unsafe { exporter.jit_run().unwrap() }
}

#[test]
fn display_test() {
    let file = std::fs::read_to_string("tests/files/fib.gon").unwrap();
    
    let lexed = tokenize(&file).unwrap();
    let program: Program = parse(&lexed).unwrap();

    println!("{program}");
}

#[test]
#[ignore]
fn lexical_scope_c_test() -> ExitCode {
    compile_and_run("tests/files/lexical_scope_c.gon")
}