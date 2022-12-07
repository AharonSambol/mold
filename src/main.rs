mod ast_add_types;
mod ast_structure;
mod mold_ast;
mod mold_tokens;
mod play_ground;
mod to_python;
mod to_rust;
mod types;

use crate::ast_structure::{join, Ast};
use crate::mold_ast::PPT;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

static mut IS_COMPILED: bool = false;

fn main() {
    // todo remove
    unsafe { IS_COMPILED = true; }

    for argument in env::args() {
        if argument == "compile" {
            unsafe {
                IS_COMPILED = true;
            }
        }
    }
    let data = fs::read_to_string("input_program.py").expect("Couldn't read file");
    let ppt = {
        PPT::new(
            Box::new(|(vc, pos)| {
                if let Some(t) = &vc[*pos].typ {
                    format!("{}\n:{t}", vc[*pos].value.to_string())
                } else {
                    vc[*pos].value.to_string()
                }
            }),
            Box::new(|(vc, pos)| {
                let children = vc.iter().nth(*pos).unwrap().clone().children;
                if let None = children {
                    return Vec::new();
                }
                children.unwrap().iter().map(|x| (vc.clone(), *x)).collect()
            }),
        )
    };
    let tokens = mold_tokens::tokenize(data);
    // println!("{:?}", tokens.iter().enumerate().collect::<Vec<(usize, &SolidToken)>>());
    let ast = mold_ast::construct_ast(&tokens, 0, &ppt).1;

    if unsafe { IS_COMPILED } {
        compile(&ast)
    } else {
        interpret(&ast);
    }
}

fn interpret(ast: &Vec<Ast>) {
    let mut py = String::new();
    to_python::to_python(&ast, 0, 0, &mut py);
    py = format!(
        r#"from typing import *
from copy import deepcopy

def __cpy_strct(x):
    return deepcopy(x) if hasattr(x, "_is_STRUCT__") else x


{}
if __name__ == '__main__':
    main()"#,
        py
    );
    println!("{py}");
    println!("\n\x1b[100m\x1b[1m\x1b[92m OUTPUT: \x1b[0m");

    let mut output = Command::new("python3")
        .arg("-c")
        .arg(format!("{py}"))
        .spawn()
        .expect("ls command failed to start");
    output.wait().unwrap();
}

fn compile(ast: &Vec<Ast>) {
    let mut rs = String::new();
    let mut enums = HashMap::new();
    to_rust::to_rust(&ast, 0, 0, &mut rs, &mut enums);
    println!("\n{}", join(&enums.values().collect(), "\n\n"));
    println!("\n{rs}");

    if !Path::new("/out").exists() {
        Command::new("cargo")
            .arg("new")
            .arg("out")
            .output()
            .expect("ls command failed to start");
    }
    let mut file = File::create("out/src/main.rs").unwrap();
    file.write_all(format!("{}\n\n{rs}", join(&enums.values().collect(), "\n\n")).as_ref())
        .unwrap();

    let check = Command::new("cargo")
        .arg("check")
        .current_dir("out")
        .output()
        .unwrap();

    if check.status.success() {
        let mut a = Command::new("cargo")
            .arg("build")
            .args(["--release", "--quiet", "--color", "always"])
            .current_dir("out")
            .spawn()
            .expect("ls command failed to start");
        a.wait().unwrap();
        println!("\n\x1b[100m\x1b[1m\x1b[92m OUTPUT: \x1b[0m");
        let mut prs = Command::new("out/target/release/out").spawn().unwrap();
        prs.wait().unwrap();
    } else {
        println!("{}", std::str::from_utf8(&check.stderr).unwrap());
    }
}
