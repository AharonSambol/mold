#![allow(clippy::too_many_arguments, clippy::only_used_in_recursion)]
mod construct_ast;
mod mold_tokens;
#[allow(warnings, unused)] mod play_ground;
mod to_python;
mod to_rust;
mod types;
mod built_in_funcs;
mod macros;
mod add_types;
mod copy_folder;

use construct_ast::ast_structure::{Ast, join};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use once_cell::sync::Lazy;
use crate::built_in_funcs::put_at_start;
use crate::construct_ast::mold_ast;
use crate::construct_ast::mold_ast::{FileInfo, Info};
use crate::construct_ast::tree_utils::clone_sub_tree;
use crate::copy_folder::CopyFolder;
use crate::mold_tokens::SolidToken;
use crate::to_python::ToWrapVal;

static mut IS_COMPILED: bool = false;
static mut IGNORE_TRAITS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_STRUCTS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_FUNCS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_ENUMS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut PARSED_FILES: Lazy<HashMap<String, FileInfo>> = Lazy::new(HashMap::new);
static mut PARSING_FILES: Lazy<HashSet<String>> = Lazy::new(HashSet::new);
static mut MODULE_PATH: Option<PathBuf> = None;

const EMPTY_STR: String = String::new();


// 2 optimizations:
// lto = "fat"
// codegen-units = 1
// TODO some things dont necessarily need to be made into a box (e.g. lst.len())
fn main() {
    // todo remove
    unsafe {
        IS_COMPILED = true;
    }
    let mut path = None;
    for argument in env::args() {
        if argument == "compile"    { unsafe { IS_COMPILED = true; } }
        else if path.is_none()      { path = Some(argument); }
        else { panic!("unexpected argument `{argument}`") }
    }
    let path = path.unwrap_or_else(|| String::from("."));

    // let mut path = String::from("tests/input_program.mo");
    let path = String::from("tests/import_package/imports.mo");
    let path = fs::canonicalize(path).unwrap().to_str().unwrap().to_string();
    // let mut path = String::from("tests/built_ins.mo");
    // let mut path = String::from("tests/enums.mo");
    // let mut path = String::from("tests/pointers.mo");
    // let mut path = String::from("tests/generics.mo");
    // let mut path = String::from("tests/lists.mo");
    // let mut path = String::from("tests/algos.mo");
    // todo find this:

    let module_path_buf = find_module_path(&path);
    let module_path = module_path_buf.to_str().unwrap().to_string();
    unsafe {
        MODULE_PATH = Some(module_path_buf);
    }
    let temp_path = if unsafe { IS_COMPILED } {
        format!("out/src/{}", module_path.rsplit_once('/').unwrap().1) // todo windows is \
    } else {
        format!("{}_TEMP_", module_path)
    };
    let path = if unsafe { IS_COMPILED } {
        println!("path: {path}, module_path: {module_path}, temp: {temp_path}");
        let res = path.replacen(&module_path, &temp_path, 1).replacen(".mo", ".rs", 1);
        println!("RES: {res}");
        res
    } else {
        path.replacen(".mo", ".py", 1)
    };
    let copy_folder = CopyFolder {
        temp_path,
        module_path,
    };
    copy_folder.start(&path);

    if !unsafe { IS_COMPILED } {
        let python_built_ins = {"class built_in_list_(list):
    def _iter_(self): return iter(pointer_(value_(x)) for x in super().__iter__())
    def append(self, val): return super().append(val.v)
    def getattr(self, attr): return self.__getattribute__(attr)
    def setattr(self, name, val): return self.__setattr__(name, val)


class value_:
    def __init__(self, v):
        self.v = v

    def getattr(self, attr):
        if hasattr(self, attr):
            return self.__getattribute__(attr)
        if isinstance(self.v, (value_, pointer_)):
            return self.v.getattr(attr)
        return self.v.__getattribute__(attr)

    def setattr(self, name, val):
        if hasattr(self, name):
            return self.__setattr__(name, val)
        if isinstance(self.v, (value_, pointer_)):
            return self.v.setattr(name, val)
        return self.v.__setattr__(name, val)

    def _dereference_all(self):
        if hasattr(self.v, '_dereference_all'):
            return self.v._dereference_all()
        return self.v

    def __str__(self): return f'{self.v}'
    def __getitem__(self, pos): return self.v.__getitem__(pos)
    def __setitem__(self, pos, val): return self.v.__setitem__(pos, val)
    def __eq__(self, other): return self.v == other.v
    def __le__(self, other): return self.v <= other.v
    def __lt__(self, other): return self.v < other.v
    def __ge__(self, other): return self.v >= other.v
    def __gt__(self, other): return self.v > other.v

class pointer_:
    def __init__(self, p):
        self.p = p

    def getattr(self, attr):
        if hasattr(self, attr):
            return self.__getattribute__(attr)
        return self.p.getattr(attr)

    def setattr(self, name, val):  # todo only if is &mut
        if hasattr(self, name):
            return self.__setattr__(name, val)
        return self.p.setattr(name, val)

    def _dereference_all(self):
        if hasattr(self.p, '_dereference_all'):
            return self.p._dereference_all()
        return self.p

    def __str__(self): return f'&{self.p}'
    def __getitem__(self, pos): return self.p.__getitem__(pos)
    def __setitem__(self, pos, val): return self.p.__setitem__(pos, val)  # todo only if is &mut
    def __eq__(self, other): return self.p == other.p
    def __le__(self, other): return self.p <= other.p
    def __lt__(self, other): return self.p < other.p
    def __ge__(self, other): return self.p >= other.p
    def __gt__(self, other): return self.p > other.p
"};
        let mut built_ins = File::create(
            format!("{}/mold_core_built_ins.py", copy_folder.module_path) // todo windows is \
        ).unwrap();
        built_ins.write_all(python_built_ins.as_ref()).unwrap();
    }

    parse_file(&path);
    // if unsafe { IS_COMPILED } {
    //     fs::rename(
    //         path.clone(),
    //         format!("{}/main.rs", path.rsplit_once('/').unwrap().0)
    //     ).unwrap(); //todo windows \
    // }
    run(&path);

    drop(copy_folder);
}

fn parse_file(path: &String) {
    println!("parsing: {path}");
    unsafe {
        if PARSING_FILES.contains(path) {
            panic!("circular import")
        }
        PARSING_FILES.insert(path.clone());
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
        one_of_enums: &mut Default::default(),
        types: &mut Default::default(),
        generics: &mut vec![],
        struct_inner_types: &mut Default::default(),
        cur_file_path: &mut PathBuf::from(path), // todo windows \
    };
    let ast = mold_ast::construct_ast(&tokens, 0, &mut info);

    if unsafe { IS_COMPILED } {
        let (enums, code) = compile(&ast, &info);
        let mut file = File::create(path).unwrap();
        file.write_all(code.as_ref()).unwrap();
    } else {
        let mut file = File::create(path).unwrap();
        file.write_all(interpret(&ast).as_ref()).unwrap();
    }
    unsafe {
        PARSED_FILES.insert(path.clone(), FileInfo {
            funcs: info.funcs.clone(), types: info.types.clone(),

            structs:
                info.structs.iter().map(|(name, typ)|
                    (name.clone(), (typ.clone(), clone_sub_tree(
                        &ast, typ.pos,None // todo exclude the body of the functions? Some(ast[typ.pos].children.as_ref().unwrap()[2]) //1 the body
                    )))
                ).collect(),
            traits: info.traits.iter().map(|(name, typ)|
                (name.clone(), (typ.clone(), vec![ast[typ.pos].clone()]))
            ).collect(),
            enums: info.enums.iter().map(|(name, typ)|
                (name.clone(), (typ.clone(), clone_sub_tree(
                    &ast, typ.pos, None
                )))
            ).collect(),
        });
    }
}


fn run(path: &String) {
    if unsafe { IS_COMPILED } {
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
    } else {
        println!("\n\x1b[100m\x1b[1m\x1b[92m OUTPUT: \x1b[0m");
        Command::new("python3").arg(path)
            .spawn().expect("ls command failed to start")
            .wait().unwrap();
    }
}

fn interpret(ast: &[Ast]) -> String {
    let py = to_python::to_python(ast, 0, 0, ToWrapVal::Nothing);
    let py = py.trim();
    let py = { format!(
        r#"from typing import *
from copy import deepcopy
from mold_core_built_ins import *

list = built_in_list_


{}
if __name__ == '__main__':
    main()"#,
        py
    ) };
    println!("{py}");
    py
}

fn compile(ast: &[Ast], info: &Info) -> (HashMap<String, String>, String) {
    let rs = to_rust::to_rust(ast, 0, 0, info);
    // let enums = to_rust::make_enums(&enums);
    let rs = rs.trim();
    // TODO something about this... v
    let mut one_of_enums = info.one_of_enums.clone();
    one_of_enums.remove( //1 this removes `Iterator | IntoIterator` which is used for the python implementation
        "_boxof_IntoIterator_of_Item_eq_T_endof__endof___or___boxof_Iterator_of_Item_eq_T_endof__endof_"
    );
    let one_of_enums_st = join(one_of_enums.values(), "\n\n");
    println!("\n{one_of_enums_st}\n{rs}");
    // println!("\n{}", rs.split_once(CODE_END_FLAG).unwrap().0.trim_end());

    // if !Path::new("/out").exists() {
    //     Command::new("cargo")
    //         .arg("new")
    //         .arg("out")
    //         .output()
    //         .expect("ls command failed to start");
    // }
//     let cargo_contents = fs::read_to_string("out/Cargo.toml").unwrap();
//     if !cargo_contents.contains("\nlto =") && !cargo_contents.contains("\ncodegen-units = ") {
//         let cargo_contents = cargo_contents.split_once("[package]")
//             .expect("no [package] found in Cargo.toml");
//         let mut file = File::create("out/Cargo.toml").unwrap();
//
//         file.write_all(
//             format!("\
// {}[package]
// lto = \"fat\"
// codegen-units = 1{}",
//                     cargo_contents.0,
//                     cargo_contents.1
//             ).as_ref()
//         ).expect("couldn't write to cargo");
//     }
//     if !cargo_contents.contains("list_comprehension_macro = \"*\"") {
//         let cargo_contents = cargo_contents.split_once("[dependencies]")
//             .expect("no [dependencies] found in Cargo.toml");
//         let mut file = File::create("out/Cargo.toml").unwrap();
//
//         file.write_all(
//             format!("\
// {}[dependencies]
// list_comprehension_macro = \"*\"{}",
//                     cargo_contents.0,
//                     cargo_contents.1
//             ).as_ref()
//         ).expect("couldn't write to cargo");
//     }
//     let mut file = File::create("out/src/main.rs").unwrap();
    (one_of_enums, {
        format!("//#![allow(warnings, unused)]
#![allow(unused, non_camel_case_types)]

use std::slice::{{Iter, IterMut}};
use std::iter::Rev;
use std::collections::{{HashMap, HashSet}};
use std::ptr;
use list_comprehension_macro::comp;

#[inline] fn _index_mut<T>(vc: &mut Vec<T>, pos: i32) -> &mut T {{
    if pos >= 0 {{
        vc.iter_mut().nth(pos as usize)
    }} else {{
        vc.iter_mut().rev().nth(-pos as usize -1)
    }}.unwrap()
}}
#[inline] fn _index<T>(vc: &Vec<T>, pos: i32) -> &T {{
    if pos >= 0 {{
        vc.iter().nth(pos as usize)
    }} else {{
        vc.iter().rev().nth(-pos as usize -1)
    }}.unwrap()
}}

{rs}")
    })
}


fn find_module_path(path: &String) -> PathBuf {
    let full_path = fs::canonicalize(path).unwrap_or_else(|_| panic!("couldn't find full path `{path}`"));
    for parent in full_path.ancestors().skip(1) {
        for path in fs::read_dir(parent).unwrap() {
            if matches!(path.unwrap().path().extension(), Some(st) if st == "mold") {
                return parent.to_path_buf();
            }
        }
    }
    panic!("no `.mold` file found")
}

