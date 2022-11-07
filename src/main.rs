// #[macro_use]
// extern crate lazy_static;

extern crate core;

mod mold_ast;
mod mold_tokens;

use std::fs;

fn main() {
    let data = fs::read_to_string("input_program.txt")
        .expect("Couldn't read file");
    // println!("{}", data);
    let tokens = mold_tokens::tokenize(data);
    println!("{:?}", tokens);
    // let ast = mold_ast::maks_ast(tokens);
}
