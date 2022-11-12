// #[macro_use]
// extern crate lazy_static;

use pretty_print_tree::{Color, PrettyPrintTree};

mod mold_ast;
mod mold_tokens;
mod ast_structure;
mod types;

use std::fs;

fn main() {
    let data = fs::read_to_string("input_program.txt")
        .expect("Couldn't read file");
    // println!("{}", data);
    let ppt = PrettyPrintTree::<(Vec<ast_structure::Ast>, usize)>::new(
        Box::new(|(vc, pos)| vc[*pos].value.to_string()),
        Box::new(|(vc, pos)| {
            let children = vc.iter()
                .nth(*pos)
                .unwrap()
                .clone()
                .children;
            if let None = children { return Vec::new(); }
            children.unwrap()
                .iter()
                .map(|x| (vc.clone(), *x))
                .collect()
        })
    );
    let tokens = mold_tokens::tokenize(data);
    println!("{:?}", tokens);
    let _ast = mold_ast::construct_ast(&tokens, 0, &ppt);
}
