#![allow(clippy::too_many_arguments)]

mod construct_ast;
mod mold_tokens;
#[allow(warnings, unused)] mod play_ground;
mod to_python;
mod to_rust;
mod types;
mod built_in_funcs;
mod macros;
mod add_types;

use construct_ast::ast_structure::{Ast, join};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use once_cell::sync::Lazy;
use crate::built_in_funcs::{put_at_start};
use crate::construct_ast::mold_ast;
use crate::construct_ast::mold_ast::Info;
use crate::mold_tokens::SolidToken;
use crate::to_python::ToWrapVal;

static mut IS_COMPILED: bool = false;
static mut IGNORE_TRAITS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_STRUCTS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_FUNCS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_ENUMS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);

const EMPTY_STR: String = String::new();

// 2 optimizations:
// lto = "fat"
// codegen-units = 1
// TODO some things dont need to be made into a box necessarily (e.g. when I do lst.len() it makes a box)
fn main() {
    // todo remove
    unsafe {
        // IS_COMPILED = true;
    }
    let mut path = String::from("tests/input_program.py");
    // let mut path = String::from("tests/pointers.py");
    // let mut path = String::from("tests/generics.py");
    // let mut path = String::from("tests/lists.py");
    for argument in env::args() {
        if argument == "compile" {
            unsafe {
                IS_COMPILED = true;
            }
        } else if let Some(p) = argument.strip_prefix("path=") {
            path = p.to_string();
        }
    }
    let data = fs::read_to_string(path).expect("Couldn't read file");
    let data = put_at_start(&data);
    let tokens = mold_tokens::tokenize(&data);
    println!("{:?}", tokens.iter().enumerate().collect::<Vec<(usize, &SolidToken)>>());
    let mut info = Info {
        funcs: &mut Default::default(),
        structs: &mut Default::default(),
        traits: &mut Default::default(),
        enums: &mut Default::default(),
        types: &mut Default::default(),
        generics: &mut vec![],
        struct_inner_types: &mut Default::default(),
    };
    let ast = mold_ast::construct_ast(&tokens, 0, &mut info);

    if unsafe { IS_COMPILED } {
        compile(&ast, &info)
    } else {
        interpret(&ast);
    }
}

fn interpret(ast: &[Ast]) {
    let py = to_python::to_python(ast, 0, 0, ToWrapVal::Nothing);
    let py = py.trim();
    let py = format!(
        r#"from typing import *
from copy import deepcopy


class _built_in_list_(list):
    def iter(self):
        return iter(_pointer_(_value_(x)) for x in self)

    def iter_mut(self):
        return iter(_pointer_(_value_(x)) for x in self)


list = _built_in_list_


class _value_:
    def __init__(self, v): self.v = v
    def __str__(self): return f'{{self.v}}'


class _pointer_:
    def __init__(self, p):
        self.__dict__ = {{x: y for x, y in p.__dict__.items()}}
        self.p = p
    def __str__(self): return f'&{{self.p}}'





{}
if __name__ == '__main__':
    main()"#,
        py
    );
    println!("{py}");
    println!("\n\x1b[100m\x1b[1m\x1b[92m OUTPUT: \x1b[0m");

    let mut output = Command::new("python3")
        .arg("-c")
        .arg(&py)
        .spawn()
        .expect("ls command failed to start");
    output.wait().unwrap();
}

fn compile(ast: &[Ast], info: &Info) {
    let mut rs = String::new();
    let mut enums = HashMap::new();
    to_rust::to_rust(ast, 0, 0, &mut rs, &mut enums, info);
    let rs = rs.trim();
    println!("\n{}", join(enums.values(), "\n\n"));
    println!("\n{}", rs);
    // println!("\n{}", rs.split_once(CODE_END_FLAG).unwrap().0.trim_end());

    if !Path::new("/out").exists() {
        Command::new("cargo")
            .arg("new")
            .arg("out")
            .output()
            .expect("ls command failed to start");
    }
    let cargo_contents = fs::read_to_string("out/Cargo.toml").unwrap();
    if !cargo_contents.contains("\nlto =") && !cargo_contents.contains("\ncodegen-units = ") {
        let cargo_contents = cargo_contents.split_once("[package]")
            .expect("no [package] found in Cargo.toml");
        let mut file = File::create("out/Cargo.toml").unwrap();

        file.write_all(
            format!("\
{}[package]
lto = \"fat\"
codegen-units = 1{}",
                    cargo_contents.0,
                    cargo_contents.1
            ).as_ref()
        ).expect("couldn't write to cargo");
    }
    let mut file = File::create("out/src/main.rs").unwrap();
    file.write_all(
        format!("//#![allow(warnings, unused)]
#![allow(unused, non_camel_case_types)]

use std::slice::{{Iter, IterMut}};
use std::iter::Rev;
use std::collections::{{HashMap, HashSet}};


{}
{rs}",
            join(enums.values(), "\n\n")
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
        Command::new("cargo")
            .arg("fix")
            .args(["--allow-no-vcs", "--broken-code"])
            .current_dir("out")
            .output()
            .unwrap();

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
