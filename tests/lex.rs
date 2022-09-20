use poligon_lang;
use std::io;

#[test]
fn duck_lex() -> io::Result<()> {
    let tokens = poligon_lang::lex_from_file("_test_files/duck.gon")?;

    println!("{:?}", tokens);
    Ok(())
}