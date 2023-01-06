use crate::ast_structure::Ast;
use crate::to_python::to_python;
use crate::to_rust::to_rust;
use crate::types::{unwrap_u, Type, TypeKind, INT_TYPE, GenericType, STR_TYPE, CHAR_TYPE, BOOL_TYPE, FLOAT_TYPE, MUT_STR_TYPE, ITER_NAME, TypName};
use std::collections::HashMap;
use std::fmt::Write;
use crate::{some_vec, typ_with_child};

macro_rules! get_types {
    () => {
        fn for_struct(&self) -> &Option<String> { &self.for_strct }
        fn input(&self) -> &Option<Vec<Type>> { &self.input_types }
        fn output(&self) -> &Option<Type> { &self.output_types }
    };
}
macro_rules! to_py {
    ($x: expr) => {
        fn to_str_python(&self, ast: &Vec<Ast>, res: &mut String, children: &Vec<usize>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
            write!(res, $x).unwrap();
            close_python(ast, res, children, built_ins);
        }
    };
}


// TODO  instead of doing it like this write the func\struct signatures
// TODO  in mold & put it at the start of the file then ignore it when to_rust\to_python
pub fn make_built_ins() -> HashMap<&'static str, Box<dyn BuiltIn>> {
    let mut res: HashMap<&'static str, Box<dyn BuiltIn>> = HashMap::new();

    let generic_iter = typ_with_child! {
        TypeKind::Struct(ITER_NAME),
        Type {
            kind: TypeKind::Generic(GenericType::Declaration(String::from("T"))),
            children: None,
        }
    };

    /*1 print */{
        res.insert(
            "print",
            Box::new(Print {
                input_types: some_vec![Type {
                    kind: TypeKind::Trait(TypName::Static("Display")),
                    children: None,
                }],
                output_types: None,
                for_strct: None,
            }),
        );
    }
    /*1 dprint */{
        res.insert(
            "dprint",
            Box::new(DPrint {
                input_types: some_vec![Type {
                    kind: TypeKind::Trait(TypName::Static("Debug")),
                    children: None,
                }],
                output_types: None,
                for_strct: None,
            }),
        );
    }
    /*1 range */{
        res.insert(
            "range",
            Box::new(Range {
                input_types: some_vec![
                    INT_TYPE,
                    Type {
                        kind: TypeKind::Optional,
                        children: some_vec![ INT_TYPE, INT_TYPE ],
                    }
                ],
                output_types: Some(Type {
                    kind: TypeKind::Struct(ITER_NAME),
                    children: some_vec![INT_TYPE],
                }),
                for_strct: None,
            }),
        );
    }
    /*1 rev */{
        // res.insert(
        //     "rev",
        //     Box::new(Rev {
        //         input_types: some_vec![generic_iter.clone()]),
        //         output_types: Some(generic_iter.clone()),
        //         for_strct: None,
        //     }),
        // );
    }
    /*1 enumerate */{
        res.insert(
            "enumerate",
            Box::new(Enumerate {
                input_types: some_vec![generic_iter],
                output_types: Some(Type {
                    kind: TypeKind::Struct(ITER_NAME),
                    children: some_vec![
                        Type {
                            kind: TypeKind::Tuple,
                            children: some_vec![
                                Type::new(String::from("u32")),
                                Type {
                                    kind: TypeKind::Generic(GenericType::Of(String::from("T"))),
                                    children: None,
                                }
                            ],
                        },
                    ],
                }),
                for_strct: None,
            }),
        );
    }
    /*1 str */{
        res.insert(
            "str",
            Box::new(Str {
                input_types: some_vec![
                    Type{
                        kind: TypeKind::Implements,
                        children: some_vec![Type::new(String::from("Display"))]
                    }
                ],
                output_types: Some(MUT_STR_TYPE),
                for_strct: None,
            }),
        );
    }
    /*1 int */{
        res.insert(
            "int",
            Box::new(Int {
                input_types: some_vec![
                    Type{
                        kind: TypeKind::_OneOf,
                        children: some_vec![MUT_STR_TYPE, STR_TYPE, CHAR_TYPE, BOOL_TYPE, FLOAT_TYPE]
                    }
                ],
                output_types: Some(INT_TYPE),
                for_strct: None,
            }),
        );
    }
    /*1 float */{
        res.insert(
            "float",
            Box::new(Float {
                input_types: some_vec![
                    Type{
                        kind: TypeKind::_OneOf,
                        children: some_vec![MUT_STR_TYPE, STR_TYPE, CHAR_TYPE, BOOL_TYPE, INT_TYPE]
                    }
                ],
                output_types: Some(FLOAT_TYPE),
                for_strct: None,
            }),
        );
    }

    res
}

pub trait BuiltIn {
    fn for_struct(&self) -> &Option<String>;
    fn input(&self) -> &Option<Vec<Type>>;
    fn output(&self) -> &Option<Type>;
    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    );
    fn to_str_python(&self, ast: &Vec<Ast>, res: &mut String, children: &Vec<usize>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>);
}

struct Print {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Print {
    get_types!();
    // TODO get inputs and then based off of types put {} or {:?}
    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        write!(
            res,
            "{}",
            format!(
                "println!(\"{}\", ",
                "{} "
                    .repeat(unwrap_u(&ast[children[1]].children).len())
                    .strip_suffix(' ')
                    .unwrap()
            )
        )
        .unwrap();
        close_rust(ast, res, children, built_ins, enums);
    }
    to_py!("print(");
}
struct DPrint {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for DPrint {
    get_types!();

    // TODO get inputs and then based off of types put {} or {:?}
    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        write!(
            res,
            "{}",
            format!(
                "println!(\"{}\", ",
                "{:?} "
                    .repeat(unwrap_u(&ast[children[1]].children).len())
                    .strip_suffix(' ')
                    .unwrap()
            )
        )
        .unwrap();
        close_rust(ast, res, children, built_ins, enums);
    }

    to_py!("print(");
}

struct Range {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Range {
    get_types!();

    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        let args = unwrap_u(&ast[children[1]].children);
        match args.len() {
            1 => {
                write!(res, "(0..").unwrap();
                to_rust(ast, args[0], 0, res, built_ins, enums);
                write!(res, ")").unwrap();
            },
            2 => {
                write!(res, "(").unwrap();
                to_rust(ast, args[0], 0, res, built_ins, enums);
                write!(res, "..").unwrap();
                to_rust(ast, args[1], 0, res, built_ins, enums);
                write!(res, ")").unwrap();
            },
            3 => {
                write!(res, "(").unwrap();
                to_rust(ast, args[0], 0, res, built_ins, enums);
                write!(res, "..").unwrap();
                to_rust(ast, args[1], 0, res, built_ins, enums);
                write!(res, ").step_by(").unwrap();
                to_rust(ast, args[2], 0, res, built_ins, enums);
                write!(res, ")").unwrap();
            },
            _ => panic!("too many args passed to range, expected 1-3 found {}", children.len())
        }
    }

    to_py!("range(");
}

struct Rev {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Rev {
    get_types!();

    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".rev()").unwrap();
    }

    to_py!("reversed(");
}

struct Enumerate {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Enumerate {
    get_types!();

    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".enumerate()").unwrap();
    }

    to_py!("enumerate(");
}

struct Str {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Str {
    get_types!();

    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".to_string()").unwrap();
    }

    to_py!("str(");
}

struct Int {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Int {
    get_types!();

    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".parse::<i32>().unwrap()").unwrap();
    }

    to_py!("int(");
}

struct Float {
    input_types: Option<Vec<Type>>,
    output_types: Option<Type>,
    for_strct: Option<String>,
}
impl BuiltIn for Float {
    get_types!();

    fn to_str_rust(
        &self,
        ast: &Vec<Ast>,
        res: &mut String,
        children: &Vec<usize>,
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".parse::<f32>().unwrap()").unwrap();
    }

    to_py!("float(");
}


fn close_python(ast: &Vec<Ast>, res: &mut String, children: &Vec<usize>, built_ins: &HashMap<&str, Box<dyn BuiltIn>>) {
    if children.len() > 1 {
        to_python(ast, children[1], 0, res, built_ins);
    }
    write!(res, ")").unwrap();
}
fn close_rust(
    ast: &Vec<Ast>,
    res: &mut String,
    children: &Vec<usize>,
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    enums: &mut HashMap<String, String>,
) {
    if children.len() > 1 {
        to_rust(ast, children[1], 0, res, built_ins, enums);
    }
    write!(res, ")").unwrap();
}

