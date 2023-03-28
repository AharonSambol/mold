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
use std::{env, fs, panic};
use std::backtrace::Backtrace;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::fmt::{Write as w};
use std::path::PathBuf;
use std::process::{Command, exit};
use once_cell::sync::Lazy;
use fancy_regex::Regex as FancyRegex;
use lazy_static::lazy_static;
use crate::add_types::polymorphism::escape_typ_chars;
use crate::built_in_funcs::put_at_start;
use crate::construct_ast::mold_ast;
use crate::construct_ast::mold_ast::{add_trait_to_struct, FileInfo, get_trt_strct_functions, Info, StructTypes};
use crate::construct_ast::tree_utils::{clone_sub_tree, print_tree};
use crate::copy_folder::{change_file_extensions, CopyFolder, delete_unused_files};
use crate::mold_tokens::{SolidToken, SolidTokenWPos};
use crate::to_python::ToWrapVal;
use crate::types::{Type};


const EMPTY_STR: String = String::new();
const RUST_IMPORTS: &str = "pub use std::{{\
slice::{{Iter, IterMut}}, \
iter::Rev, \
collections::{{HashMap, HashSet}}, \
ptr, \
fmt::{{Debug, Display, Formatter, Error}} }};
pub use list_comprehension_macro::comp;";

//3 This isn't exactly good practice...
static mut IS_COMPILED: bool = false;
static mut DONT_PRINT: bool = false;
static mut IGNORE_TRAITS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_STRUCTS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_FUNCS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut IGNORE_ENUMS: Lazy<HashSet<&'static str>> = Lazy::new(HashSet::new);
static mut PARSED_FILES: Lazy<HashMap<String, FileInfo>> = Lazy::new(HashMap::new);
static mut PARSING_FILES: Lazy<HashSet<String>> = Lazy::new(HashSet::new);
static mut MODULE_PATH: Option<PathBuf> = None;
static mut IMPL_TRAITS: Lazy<HashMap<ImplTraitsKey, Vec<ImplTraitsVal>>> = Lazy::new(HashMap::new);
static mut CUR_LINE: usize = 0;
static mut CUR_COL: usize = 0;
static mut CUR_PATH: Vec<String> = vec![]; //1 this is a vec cuz when importing it might change this but it should change back when done
static mut SRC_CODE: Vec<String> = vec![]; //1 this is a vec cuz when importing it might change this but it should change back when done
static mut LINE_DIFF: Vec<usize> = vec![];

lazy_static! {
    pub static ref POINTER_WITHOUT_LIFETIME: FancyRegex = FancyRegex::new(r"&(?!\s*mut)(?!\s*')").unwrap();
    pub static ref MUT_POINTER_WITHOUT_LIFETIME: FancyRegex = FancyRegex::new(r"&\s*mut(?!\s*')").unwrap();
}

type OneOfEnums = HashMap<String, OneOfEnumTypes>;

#[derive(Eq, PartialEq, Hash, Debug)]
struct ImplTraitsKey {
    name: String,
    path: String
}

#[derive(Clone, Debug)]
struct ImplTraitsVal { // TODO when adds a new Impl needs to not only check the name but also the generics
    trt_name: String,
    implementation: Implementation,
    types: Option<HashMap<String, Type>>,
    generics: Option<Vec<Type>>
}

#[derive(Clone, Debug)]
enum Implementation {
    Is(String),
    None,
    Todo(String) //1 module
}

#[derive(Debug, Clone)]
pub struct OneOfEnumTypes {
    options: Vec<Type>,
    generics: String,
    needs_lifetime: bool
}


// 2 optimizations:
// lto = "fat"
// codegen-units = 1
// todo some things dont necessarily need to be made into a box (e.g. lst.len())
// todo play with boxing stuff in the enums\structs to decrease their size
// TODO it doesnt check the types passed to built ins (i think) but it needs to for some at least
fn main() {
    panic::set_hook(Box::new(|info| {
        println!("{}", panic_message::panic_info_message(info));
        println!(
            "\n{}",
            Backtrace::force_capture().to_string()
                .split('\n').skip(12).enumerate()
                .map(|(i, x)|
                    if i % 2 == 0 { x.trim_start().to_string() }
                    else { format!("    {}", x.trim_start()) }
                ).collect::<Vec<_>>()
                .join("\n")
        ); // TODO remove this
    }));
    // todo remove
    unsafe {
        IS_COMPILED = true;
    }
    let mut test = false;
    let mut path = None;
    for argument in env::args().skip(1) {
        if argument == "compile"    { unsafe { IS_COMPILED = true; } }
        else if argument == "test"  { test = true; unsafe { DONT_PRINT = true; } }
        else if argument == "noprint" { unsafe { DONT_PRINT = true; }} // todo remove this
        else if path.is_none()      {
            // todo check that is valid path else panic (not throw)
            path = Some(argument);
        }
        else { panic!("\x1b[1m\x1b[91munexpected argument `{}`\x1b[0m", argument) }
    }
    let path = path.unwrap_or_else(|| String::from("."));

    let paths = [
        /*1     0*/ "tests/input_program.mo",
        /*1     1*/ "tests/import_package/imports.mo",
        /*1     2*/ "tests/built_ins.mo",
        /*1     3*/ "tests/enums.mo",
        /*1     4*/ "tests/pointers.mo",
        /*1     5*/ "tests/generics.mo",
        /*1     6*/ "tests/lists.mo",
        /*1     7*/ "tests/algos.mo",
        /*1     8*/ "tests/unions.mo",
        /*1     9*/ "tests/tictactoe.mo",
    ];
    if test {
        for p in paths {
            println!("\x1b[107m\x1b[1m\x1b[94m ################### {p} ################### \x1b[0m");
            run_on_path(p);
            unsafe {
                IGNORE_TRAITS.clear();  IGNORE_STRUCTS.clear(); IGNORE_FUNCS.clear();
                IGNORE_ENUMS.clear();   PARSED_FILES.clear();   PARSING_FILES.clear();
                IMPL_TRAITS.clear();
                MODULE_PATH = None;
            }
        }
    } else {
        run_on_path(paths[0]);
    }
}

fn run_on_path(path: &str) {
    let path = fs::canonicalize(path).unwrap().to_str().unwrap().to_string();

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
        path.replacen(&module_path, &temp_path, 1)
        //.replacen(".mo", ".rs", 1);
    } else {
        path.replacen(".mo", ".py", 1)
    };
    let copy_folder = CopyFolder { temp_path, module_path };
    let main = copy_folder.start();

    if !unsafe { IS_COMPILED } {
        let python_built_ins = {
"
from copy import deepcopy as _clone
def clone(x): return _clone(x.v.p)

class built_in_list_(list):
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
    def __eq__(self, other): return self.v == other
    def __le__(self, other): return self.v <= other
    def __lt__(self, other): return self.v < other
    def __ge__(self, other): return self.v >= other
    def __gt__(self, other): return self.v > other

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

    let mut one_of_enums = HashMap::new();

    parse_file(&path, &mut one_of_enums);
    if unsafe { IS_COMPILED } {
        let module_name = copy_folder.temp_path.as_str().rsplit_once('/').unwrap().1; // todo windows is \
        let file_name = path
            .strip_prefix(&copy_folder.temp_path).unwrap()
            .strip_prefix('/').unwrap()
            .replace('/', "::"); // todo windows is \
        // let file_name = file_name.strip_suffix(".rs").unwrap();
        let file_name = file_name.strip_suffix(".mo").unwrap();

        // TODO something about this  v
        one_of_enums.remove( //1 this removes `Iterator | IntoIterator` which is used for the python implementation
            "_boxof_IntoIterator_of_Item_eq_T_endof__endof___or___boxof_Iterator_of_Item_eq_T_endof__endof_"
        );
        let mut rust_main_code = { format!(
            "#![allow(unused, non_camel_case_types)]
mod {module_name};

{RUST_IMPORTS}

#[inline] fn _index_mut<T>(vc: &mut Vec<T>, pos: i32) -> &mut T {{
    if pos >= 0 {{
        vc.iter_mut().nth(pos as usize)
    }} else {{
        vc.iter_mut().rev().nth(-pos as usize -1)
    }}.unwrap()
}}
#[inline] fn _index<T>(vc: &[T], pos: i32) -> &T {{
    if pos >= 0 {{
        vc.iter().nth(pos as usize)
    }} else {{
        vc.iter().rev().nth(-pos as usize -1)
    }}.unwrap()
}}
#[inline(always)] fn clone<T: Clone>(x: &T) -> T {{
    x.clone()
}}

fn main() {{ {module_name}::{file_name}::main(); }}"
        )};
        for (enm_name, enm_types) in one_of_enums {
            let mut needs_lifetime = false;
            let elems = enm_types.options
                .iter()
                .map(|x| {
                    let x_str = x.to_string();
                    if x_str == "None" {
                        String::from("_None")
                    } else if x_str.contains('&') {
                        let new_str = MUT_POINTER_WITHOUT_LIFETIME.replace_all(&x_str, "&mut 'b_i_lifetime ");
                        let new_str = POINTER_WITHOUT_LIFETIME.replace_all(&new_str, "&'b_i_lifetime ");
                        needs_lifetime = needs_lifetime || new_str != x_str;
                        format!(
                            "_{}({new_str})",
                            escape_typ_chars(&x_str)
                        )
                    } else {
                        format!(
                            "_{}({x_str})",
                            escape_typ_chars(&x_str)
                        )
                    }
                });
            let elems = join(elems, ",");
            // let mut impls = get_type_traits(&Type {
            //     kind: TypeKind::OneOf,
            //     children: Some(types),
            // }, ast, info);
            let res = format!(
                "/*#[derive(Clone, PartialEq)]*/\npub enum {enm_name} {} {{ {elems} }}",
                if enm_types.generics.is_empty() {
                    if needs_lifetime { String::from("<'b_i_lifetime>") } else { EMPTY_STR }
                } else if needs_lifetime {
                    format!("<'b_i_lifetime, {}", enm_types.generics.strip_prefix('<').unwrap())
                } else { enm_types.generics },
                // todo!() //1 impls
            );
            writeln!(&mut rust_main_code, "{}", res).unwrap();
        }
        main.unwrap().write_all(rust_main_code.as_ref()).unwrap();

        unsafe {
            for (key, val) in IMPL_TRAITS.iter() {
                if IGNORE_STRUCTS.contains(&*key.name) || IGNORE_ENUMS.contains(&*key.name) {
                    continue
                }
                let path = &key.path;
                let mut file = OpenOptions::new()
                    .write(true)
                    .append(true)
                    .open(path)
                    .unwrap();
                for imp_trt in val {
                    match &imp_trt.implementation {
                        Implementation::Is(implementation) =>
                            writeln!(file, "{}", implementation).unwrap(),
                        Implementation::Todo(module) => {
                            //3 **** PLEASE DONT READ THIS BLOCK OF CODE ****
                            //3 VERY VERY NOT EFFICIENT AT ALL !!!! (like not even a bit)
                            //3 Please take this as an example of what NOT to do when writing code
                            let file_info = PARSED_FILES.get(module).unwrap();
                            let (_, trt_tree) = file_info.traits.get(&imp_trt.trt_name).unwrap();
                            let (_, stct_tree) = file_info.structs.get(&key.name).unwrap();
                            let trt_module = trt_tree[0].ref_children()[1];
                            let stct_module = stct_tree[0].ref_children()[2];
                            let trt_funcs = get_trt_strct_functions(trt_tree, &trt_tree[trt_module]);
                            let stct_funcs = get_trt_strct_functions(stct_tree, &stct_tree[stct_module]);
                            let fake_info = Info {
                                funcs: &mut file_info.funcs.clone(),
                                structs: &mut file_info.structs.iter().map(|(name, (typ, _))| (name.clone(), typ.clone())).collect(),
                                traits: &mut file_info.traits.iter().map(|(name, (typ, _))| (name.clone(), typ.clone())).collect(),
                                enums: &mut file_info.enums.iter().map(|(name, (typ, _))| (name.clone(), typ.clone())).collect(),
                                one_of_enums: &mut Default::default(),
                                types: &mut file_info.types.clone(),
                                generics: &mut vec![],
                                struct_associated_types: &mut Default::default(),
                                cur_file_path: &mut Default::default(),
                            };
                            // dbg!(imp_trt);
                            // throw!();
                            let implementation = add_trait_to_struct(
                                stct_tree, &key.name, &stct_funcs,
                                &imp_trt.trt_name, &trt_funcs, &imp_trt.types, &imp_trt.generics, &fake_info
                            );
                            // println!("!!!!!!!!!! {}", implementation);
                            writeln!(file, "{}", implementation).unwrap()
                        }
                        Implementation::None => unreachable!()
                    }
                }
            }
        }

        let path = PathBuf::from(&copy_folder.temp_path);
        delete_unused_files(&path);
        change_file_extensions(&path, "rs");
    }
    run(&path);

    drop(copy_folder);
}

fn parse_file(path: &String, one_of_enums: &mut OneOfEnums) {
    unsafe {
        if PARSING_FILES.contains(path) {
            throw!("circular import")
        }
        PARSING_FILES.insert(path.clone());
    }

    let orig_data = fs::read_to_string(path).expect("Couldn't read file");
    let data = put_at_start(&orig_data);
    unsafe {
        SRC_CODE.push(data.clone());
        CUR_PATH.push(path.clone());
        LINE_DIFF.push(
            data.chars().filter(|x| *x == '\n').count()
                - orig_data.chars().filter(|x| *x == '\n').count()
        );
    }

    for i in 0..150 {
        println!("{i}: \x1b[{i}m Exception \x1b[0m ")
    }
    // panic2!("!");

    let tokens = mold_tokens::tokenize(&data);
    if unsafe { !DONT_PRINT } {
        println!("{:?}", tokens.iter().enumerate().collect::<Vec<(usize, &SolidTokenWPos)>>());
    }
    let mut info = Info {
        funcs: &mut Default::default(),
        structs: &mut Default::default(),
        traits: &mut Default::default(),
        enums: &mut Default::default(),
        one_of_enums,
        types: &mut Default::default(),
        generics: &mut vec![],
        struct_associated_types: &mut Default::default(),
        cur_file_path: &mut PathBuf::from(path), // todo windows \
    };
    let ast = mold_ast::construct_ast(&tokens, 0, &mut info);

    if unsafe { IS_COMPILED } {
        let (enums, code) = compile(&ast, &info);
        let mut file = File::create(path).unwrap();
        file.write_all(format!("
{RUST_IMPORTS}
use crate::{{ _index_mut, _index, clone, {} }};

{code}",
            join(enums.keys(), ",")
        ).as_ref()).unwrap();
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
                        &ast, typ.pos, None // todo exclude the body of the functions? Some(ast[typ.pos].ref_children()[2]) //1 the body
                    )))
                ).collect(),
            traits: info.traits.iter().map(|(name, typ)|
                (name.clone(), (typ.clone(), clone_sub_tree(
                    &ast, typ.pos, None // todo exclude the body of the functions? Some(ast[typ.pos].ref_children()[2]) //1 the body
                )))
            ).collect(),
            enums: info.enums.iter().map(|(name, typ)|
                (name.clone(), (typ.clone(), clone_sub_tree(
                    &ast, typ.pos, None
                )))
            ).collect(),
        });
    }
    unsafe { SRC_CODE.pop(); CUR_PATH.pop(); LINE_DIFF.pop(); }
    // println!("{}", unsafe { SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap() });
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
from mold_core_built_ins import *

list = built_in_list_


{}
if __name__ == '__main__':
    main()"#,
        py
    ) };
    if unsafe { !DONT_PRINT } {
        println!("{py}");
    }
    py
}

fn compile(ast: &[Ast], info: &Info) -> (OneOfEnums, String) {
    let rs = to_rust::to_rust(ast, 0, 0, info);
    let rs = rs.trim();
    // TODO something about this... v
    let mut one_of_enums = info.one_of_enums.clone();
    one_of_enums.remove( //1 this removes `Iterator | IntoIterator` which is used for the python implementation
        "_boxof_IntoIterator_of_Item_eq_T_endof__endof___or___boxof_Iterator_of_Item_eq_T_endof__endof_"
    );
    // let one_of_enums_st = join(one_of_enums.values(), "\n\n");
    // println!("\n{one_of_enums_st}\n{rs}");
    if unsafe { !DONT_PRINT } {
        println!("{rs}");
    }
    /*
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
    if !cargo_contents.contains("list_comprehension_macro = \"*\"") {
        let cargo_contents = cargo_contents.split_once("[dependencies]")
            .expect("no [dependencies] found in Cargo.toml");
        let mut file = File::create("out/Cargo.toml").unwrap();

        file.write_all(
            format!("\
{}[dependencies]
list_comprehension_macro = \"*\"{}",
                    cargo_contents.0,
                    cargo_contents.1
            ).as_ref()
        ).expect("couldn't write to cargo");
    }
    let mut file = File::create("out/src/main.rs").unwrap();
    */
    (one_of_enums, rs.to_string())
}


fn find_module_path(path: &String) -> PathBuf {
    let full_path = fs::canonicalize(path).unwrap_or_else(|_| throw!("couldn't find full path `{}`", path));
    for parent in full_path.ancestors().skip(1) {
        for path in fs::read_dir(parent).unwrap() {
            if matches!(path.unwrap().path().extension(), Some(st) if st == "mold") {
                return parent.to_path_buf();
            }
        }
    }
    throw!("no `.mold` file found")
}

