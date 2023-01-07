#![allow(clippy::too_many_arguments)]
mod ast_add_types;
mod ast_structure;
mod mold_ast;
mod mold_tokens;
#[allow(warnings, unused)] mod play_ground;
mod to_python;
mod to_rust;
mod types;
mod built_in_funcs;
mod macros;

use crate::ast_structure::{join, Ast};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write as W;
use std::fmt::Write;
use std::path::Path;
use std::process::Command;
use once_cell::sync::Lazy;
use pretty_print_tree::PrettyPrintTree;
use crate::built_in_funcs::{BuiltIn, make_built_ins};
use crate::mold_tokens::SolidToken;

static mut IS_COMPILED: bool = false;
static mut IGNORE_STRUCTS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_FUNCS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);

// 2 optimizations:
// lto = "fat"
// codegen-units = 1
//
// TODO doesnt seem to check that func\struct that takes 2 of same generic are actually same typ
fn main() {
    // todo remove
    unsafe {
        IS_COMPILED = true;
    }
    // let mut path = String::from("tests/input_program.py");
    let mut path = String::from("tests/generics.py");
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
    let mut data = fs::read_to_string(path).expect("Couldn't read file");
    put_at_start(&mut data);
    let tokens = mold_tokens::tokenize(data);
    println!("{:?}", tokens.iter().enumerate().collect::<Vec<(usize, &SolidToken)>>());

    let built_ins = make_built_ins();

    let ast = mold_ast::construct_ast(&tokens, 0, &built_ins).1;

    if unsafe { IS_COMPILED } {
        compile(&ast, &built_ins)
    } else {
        interpret(&ast, &built_ins);
    }
}

fn interpret(ast: &Vec<Ast>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
    let mut py = String::new();
    to_python::to_python(ast, 0, 0, &mut py, built_ins);
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
        .arg(&py)
        .spawn()
        .expect("ls command failed to start");
    output.wait().unwrap();
}

fn compile(ast: &Vec<Ast>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
    let mut rs = String::new();
    let mut enums = HashMap::new();
    to_rust::to_rust(ast, 0, 0, &mut rs, built_ins, &mut enums);
    println!("\n{}", join(enums.values(), "\n\n"));
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
        format!("#![allow(warnings, unused)]

use std::slice::{{Iter, IterMut}};
use std::iter::Rev;


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



enum StructFunc { Struct(BuiltInStruct), Func(BuiltInFunc) }
struct BuiltInStruct {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    methods: Vec<&'static str>,
    static_methods: Vec<&'static str>,
    _parameters: Vec<&'static str>
}
struct BuiltInFunc {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    args: Vec<&'static str>,
    return_typ: Option<&'static str>
}
fn put_at_start(data: &mut String) {
    let to_add = [
        make_primitive!(i8),  make_primitive!(i16), make_primitive!(i32),
        make_primitive!(i64), make_primitive!(i128), make_primitive!(isize),
        make_primitive!(u8),  make_primitive!(u16), make_primitive!(u32),
        make_primitive!(u64), make_primitive!(u128), make_primitive!(usize),
        make_primitive!(f32), make_primitive!(f64),
        make_primitive!(bool), make_primitive!(str), make_primitive!(char),
        //1 String
        StructFunc::Struct(BuiltInStruct{
            name: "String",
            generics: None,
            methods: vec![
                "clone() -> str", // todo do automatically?
                "split(s: str) -> List[str]", //todo (optional) s: str | char    if rust: -> Iter[str]
                "strip() -> str",        //todo (optional) c: char
                "lstrip() -> str",        //todo (optional) c: char
                "rstrip() -> str",        //todo (optional) c: char
                "len() -> int",         //todo -> usize technically
                "contains(s: str) -> bool", //todo s: str | char
                "replace(orig: str, new: str) -> str", //todo orig/new: str | char
                "startswith(s: str) -> bool",
                "endswith(s: str) -> bool",
                "find(s: str) -> int",  //todo s: str | char
                "count(s: str) -> int",  //todo s: str | char
                "removeprefix(s: str) -> str",
                "removesuffix(s: str) -> str",
                "lower(s: str) -> str",
                "upper(s: str) -> str",
                // todo chars()
                // todo is(digit\numeric\ascii...)
                // todo "join(lst: List[T]) -> int",
            ],
            static_methods: vec![],
            _parameters: vec![],
        }),
        //1 Iter
        StructFunc::Struct(BuiltInStruct{
            name: "Iter",
            generics: Some(vec!["T"]),
            methods: vec![
                "into_iter() -> IntoIter[T]",
            ],
            static_methods: vec![],
            _parameters: vec![],
        }),
        //1 IntoIter
        StructFunc::Struct(BuiltInStruct{
            name: "IntoIter",
            generics: Some(vec!["T"]),
            methods: vec![
                "into_iter() -> IntoIter[T]",
                "next() -> T"
            ],
            static_methods: vec![],
            _parameters: vec![],
        }),
        //1 IterMut
        StructFunc::Struct(BuiltInStruct{
            name: "IterMut",
            generics: Some(vec!["T"]),
            methods: vec![
                "into_iter() -> IntoIter[T]",
            ],
            static_methods: vec![],
            _parameters: vec![],
        }),
        //1 Box
        StructFunc::Struct(BuiltInStruct{
            name: "Box",
            generics: Some(vec!["T"]),
            methods: vec![],
            static_methods: vec![
                "new(t: T) -> Box[T]",
            ],
            _parameters: vec![],
        }),
        //1 Vec
        StructFunc::Struct(BuiltInStruct{
            name: "Vec",
            generics: Some(vec!["T"]),
            methods: vec![
                "into_iter() -> IntoIter[T]",
                "iter_mut() -> IterMut[T]",
                "iter() -> Iter[T]",
                "append(t: T)",
                "index(pos: usize) -> T",
            ],
            static_methods: vec![
                "new() -> Vec[T]"
            ],
            _parameters: vec![],
        }),
        /* //1 Rev
        StructFunc::Struct(BuiltInStruct{
            name: "Rev",
            generics: Some(vec!["T"]), // this should be Iter[T]
            methods: vec![
                "into_iter() -> IntoIter[T::Item]", //3 this is what's wrong
                "iter() -> Iter[T]",
            ],
            _parameters: vec![],
        }),
        //1 reversed
        StructFunc::Func(BuiltInFunc{
            name: "reversed",
            generics: Some(vec!["T"]),
            args: vec!["t: Iter[T]"],
            return_typ: Some("Rev[Iter[T]]"),
        }),
         */
    ];
    writeln!(data).unwrap();
    for add in to_add {
        match add {
            StructFunc::Struct(stct) => {
                unsafe {
                    IGNORE_STRUCTS.insert(stct.name);
                }
                if let Some(generics) = stct.generics {
                    write!(data, "struct {}<{}>:", stct.name, join(generics.iter(), ",")).unwrap();
                } else {
                    write!(data, "struct {}:", stct.name).unwrap();
                }
                // todo parameters
                for func in stct.methods {
                    write!(data, "\n\tdef {}:", func).unwrap();
                }
                for func in stct.static_methods {
                    write!(data, "\n\tstatic def {}:", func).unwrap();
                }
                writeln!(data).unwrap();
            },
            StructFunc::Func(func) => {
                unsafe {
                    IGNORE_FUNCS.insert(func.name);
                }
                let args = join(func.args.iter(), ",");
                let rtrn = if let Some(t) = func.return_typ {
                    format!("-> {t}")
                } else {
                    String::new()
                };
                let generics = if let Some(generics) = func.generics {
                    format!("<{}>", join(generics.iter(), ","))
                } else {
                    String::new()
                };
                write!(data, "def {}{generics}({args}){rtrn}:", func.name).unwrap();
                writeln!(data).unwrap();
            },
        }
    }
    // println!("{data}");
}

pub fn print_tree(tree: (Vec<Ast>, usize)){
    let ppt = {
        PrettyPrintTree::<(Vec<Ast>, usize)>::new(
            Box::new(|(vc, pos)| {
                if let Some(t) = &vc[*pos].typ {
                    format!("{}\n:{t}", vc[*pos].value)
                } else {
                    vc[*pos].value.to_string()
                }
            }),
            Box::new(|(vc, pos)| {
                let children = vc.get(*pos).unwrap().clone().children;
                if children.is_none() {
                    return Vec::new();
                }
                children.unwrap().iter().map(|x| (vc.clone(), *x)).collect()
            }),
        )
    };
    println!("{}\n", ppt.to_str(&tree));
}

