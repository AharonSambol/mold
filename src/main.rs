mod ast_add_types;
mod ast_structure;
mod mold_ast;
mod mold_tokens;
mod play_ground;
mod to_python;
mod to_rust;
mod types;
mod built_in_funcs;
mod macros;

use crate::ast_structure::{join, Ast, AstNode};
use crate::mold_ast::PPT;
use std::collections::{HashMap, HashSet};
use std::{env, io, mem};
use std::fs;
use std::fs::File;
use std::io::Write as W;
use std::fmt::Write;
use std::path::Path;
use std::process::Command;
use once_cell::sync::Lazy;
use crate::built_in_funcs::{BuiltIn, make_built_ins};
use crate::mold_tokens::SolidToken;

static mut IS_COMPILED: bool = false;
static mut IGNORE_STRUCTS: Lazy<HashSet<&'static str>> = Lazy::new(|| HashSet::new());


fn main() {
    // todo remove
    unsafe {
        IS_COMPILED = true;
    }

    for argument in env::args() {
        if argument == "compile" {
            unsafe {
                IS_COMPILED = true;
            }
        }
    }
    let mut data = fs::read_to_string("input_program.py").expect("Couldn't read file");
    put_at_start(&mut data);
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
    println!("{:?}", tokens.iter().enumerate().collect::<Vec<(usize, &SolidToken)>>());

    let built_ins = make_built_ins();

    let ast = mold_ast::construct_ast(&tokens, 0, &ppt, &built_ins).1;

    if unsafe { IS_COMPILED } {
        compile(&ast, &built_ins)
    } else {
        interpret(&ast, &built_ins);
    }
}

fn interpret(ast: &Vec<Ast>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
    let mut py = String::new();
    to_python::to_python(&ast, 0, 0, &mut py, &built_ins);
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

fn compile(ast: &Vec<Ast>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
    let mut rs = String::new();
    let mut enums = HashMap::new();
    to_rust::to_rust(&ast, 0, 0, &mut rs, built_ins, &mut enums);
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
    file.write_all(
        format!(
            "use std::slice::Iter;
{}
{rs}",
            join(&enums.values().collect(), "\n\n")
        )
        .as_ref(),
    )
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

enum StructFunc { Struct, Func }
fn put_at_start(data: &mut String) {
    let to_add = [
        (StructFunc::Struct, "String", None, vec![
            "split(s: str) -> List[str]", //TODO (optional) s: str | char    if rust: -> Iter[str]
            "strip() -> str",        //TODO (optional) c: char
            "lstrip() -> str",        //TODO (optional) c: char
            "rstrip() -> str",        //TODO (optional) c: char
            "len() -> int",         //TODO -> usize technically
            "contains(s: str) -> bool", //TODO s: str | char
            "replace(orig: str, new: str) -> str", //TODO orig/new: str | char
            "startswith(s: str) -> bool",
            "endswith(s: str) -> bool",
            "find(s: str) -> int",  //TODO s: str | char
            "count(s: str) -> int",  //TODO s: str | char
            "removeprefix(s: str) -> str",
            "removesuffix(s: str) -> str",
            "lower(s: str) -> str",
            "upper(s: str) -> str",
            // todo is(digit\numeric\ascii...)
            // todo "join(lst: List[T]) -> int",
        ]),
        (StructFunc::Struct, "Vec", Some(vec!["T"]), vec![
            "into_iter() -> T"
        ])
    ];
    write!(data, "\n").unwrap();
    for add in to_add {
        match add.0 {
            StructFunc::Struct => {
                unsafe {
                    IGNORE_STRUCTS.insert(add.1);
                }
                if let Some(generics) = add.2 {
                    write!(data, "struct {}<{}>:", add.1, join(&generics, ",")).unwrap();
                } else {
                    write!(data, "struct {}:", add.1).unwrap();
                }
                for func in add.3 {
                    write!(data, "\n\tdef {}:", func).unwrap();
                }
                writeln!(data).unwrap();
            },
            StructFunc::Func => todo!(),
        }
    }
}