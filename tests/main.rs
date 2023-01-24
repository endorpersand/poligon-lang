use std::fs;
use std::path::Path;

use inkwell::context::Context;
use poligon_lang::compiler::{codegen, Compiler};
use poligon_lang::interpreter::runtime::value::Value;
use poligon_lang::{Interpreter, lexer, parser};

fn compile_and_run(fp: impl AsRef<Path>) {
    let code   = fs::read_to_string(fp).unwrap();
    let lexed  = lexer::tokenize(&code).unwrap();
    let parsed = parser::parse(lexed).unwrap();
    let plir   = codegen::codegen(parsed).unwrap();

    let ctx = Context::create();
    let mut compiler = Compiler::from_ctx(&ctx);
    
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
    let result = Interpreter::from_file("tests/files/lexical_scope_i.gon").unwrap()
        .run()
        .unwrap();

    let val @ Value::List(_) = result else {
        panic!("Test did not return list, it returned {}", result.repr());
    };

    let v0 = val.get_index(Value::Int(0));
    let v1 = val.get_index(Value::Int(1));

    assert!(v0.is_ok());
    assert!(v1.is_ok());
    assert_eq!(v0.unwrap(), Value::Str("global".to_string()));
    assert_eq!(v1.unwrap(), Value::Str("global".to_string()));
    assert!(val.get_index(Value::Int(2)).is_err());
}

#[test]
fn lexical_scope_c_test() {
    compile_and_run("tests/files/lexical_scope_c.gon");
}