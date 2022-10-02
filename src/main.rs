use std::io;

use poligon_lang::*;

fn main() -> io::Result<()> {
    let parsed = parse_from_file("_test_files/example.gon")?;
    println!("{:?}", parsed);
    Ok(())
}