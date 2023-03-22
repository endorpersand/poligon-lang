use std::collections::VecDeque;
use std::fs;
use std::io::{BufReader, BufRead};
use std::path::Path;

use inkwell::context::Context;
use poligon_lang::compiler::{plir_codegen, LLVMCodegen};
use poligon_lang::interpreter::runtime::IoHook;
use poligon_lang::{Interpreter, lexer, parser};

fn compile_and_run(fp: impl AsRef<Path>) {
    let code   = fs::read_to_string(fp).unwrap();
    let lexed  = lexer::tokenize(&code).unwrap();
    let parsed = parser::parse(lexed).unwrap();
    let plir   = plir_codegen::plir_codegen(parsed).unwrap();

    let ctx = Context::create();
    let mut compiler = LLVMCodegen::new(&ctx);
    
    let main = compiler.compile(&plir).unwrap();
    unsafe { compiler.jit_run::<()>(main).unwrap(); }
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
fn lexical_scope_c_test() {
    compile_and_run("tests/files/lexical_scope_c.gon");
}