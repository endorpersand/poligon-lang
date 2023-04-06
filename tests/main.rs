use std::collections::VecDeque;
use std::io::{BufReader, BufRead};
use std::path::Path;
use std::process::ExitCode;

use inkwell::context::Context;
use poligon_lang::compiler::Compiler;
use poligon_lang::interpreter::runtime::IoHook;
use poligon_lang::Interpreter;

fn compile_and_run(fp: impl AsRef<Path>) -> ExitCode {
    let ctx = Context::create();

    let path = fp.as_ref();
    let name = path.file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let mut compiler = Compiler::new(&ctx, name).unwrap();
    compiler.load_gon_file(path).unwrap();
    unsafe { compiler.jit_run().unwrap() }
}

#[test]
fn display_test() {
    let ir = Interpreter::from_file("tests/files/fib.gon").unwrap();

    let program = ir.parse().unwrap();
    println!("{program}");
}

#[test]
fn lexical_scope_i_test() {
    let mut dq = VecDeque::new();
    
    let hook = IoHook::new_w(&mut dq);
    let ir = Interpreter::from_file("tests/files/lexical_scope_i.gon").unwrap();
    ir.run_with_io(hook).unwrap();

    let reader = BufReader::new(dq);
    let mut lines = reader.lines();
    
    assert_eq!(lines.next().unwrap().unwrap(), "global");
    assert_eq!(lines.next().unwrap().unwrap(), "global");
    assert!(lines.next().is_none());
}

#[test]
#[ignore]
fn lexical_scope_c_test() -> ExitCode {
    compile_and_run("tests/files/lexical_scope_c.gon")
}