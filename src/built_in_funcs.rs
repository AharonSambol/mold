use crate::construct_ast::ast_structure::{Ast, join};
use crate::to_python::{to_python, ToWrapVal};
use crate::to_rust::to_rust;
use crate::types::{unwrap_u, Type, TypeKind, INT_TYPE, GenericType, STR_TYPE, CHAR_TYPE, BOOL_TYPE, FLOAT_TYPE, MUT_STR_TYPE, TypName, ITER_TYPE};
use std::collections::HashMap;
use std::fmt::Write;
use crate::{IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, make_primitive, some_vec, typ_with_child};

macro_rules! get_types {
    () => {
        fn for_struct(&self) -> &Option<String> { &self.for_strct }
        fn input(&self) -> &Option<Vec<Type>> { &self.input_types }
        fn output(&self) -> &Option<Type> { &self.output_types }
    };
}
macro_rules! to_py {
    ($x: expr) => {
        fn to_str_python(
            &self, ast: &[Ast], children: &[usize],
            built_ins: &HashMap<&str, Box<dyn BuiltIn>>
        ) -> String {
            format!($x,
                close_python(ast, children, built_ins, false)
            )
        }
    };
}




enum StructFunc { Struct(BuiltInStruct), Func(BuiltInFunc), Trait(BuiltInTrait) }
struct BuiltInStruct {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    methods: Vec<&'static str>,
    _parameters: Vec<&'static str>
}
struct BuiltInTrait {
    name: &'static str,
    duck: bool,
    generics: Option<Vec<&'static str>>,
    methods: Vec<&'static str>,
    types: Option<Vec<&'static str>>,
    _parameters: Vec<&'static str>,
    ignore: bool
}
struct BuiltInFunc {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    args: Vec<&'static str>,
    return_typ: Option<&'static str>
}

pub fn put_at_start(input: &str) -> String {
    let mut data = String::new();
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
                "__init__(self)"
                // todo chars()
                // todo is(digit\numeric\ascii...)
                // todo "join(lst: List[T]) -> int",
            ],
            _parameters: vec![],
        }),
        //1 Iter
        StructFunc::Struct(BuiltInStruct{
            name: "Iter",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "into_iter() -> IntoIter[T]",
            ],
            _parameters: vec![],
        }),
        //1 IntoIter
        StructFunc::Struct(BuiltInStruct{
            name: "IntoIter",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "into_iter() -> IntoIter[T]",
                "next() -> T"
            ],
            _parameters: vec![],
        }),
        //1 IterMut
        StructFunc::Struct(BuiltInStruct{
            name: "IterMut",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "into_iter() -> IntoIter[T]",
            ],
            _parameters: vec![],
        }),
        //1 Box
        StructFunc::Struct(BuiltInStruct{
            name: "Box",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "new(t: T) -> Box[T]",
            ],
            _parameters: vec![],
        }),
        //1 Vec
        StructFunc::Struct(BuiltInStruct{
            name: "Vec",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "into_iter() -> IntoIter[T]",
                "iter_mut() -> IterMut[T]",
                "iter() -> Iter[T]",
                "append(t: T)",
                "index(pos: usize) -> T",
            ],
            _parameters: vec![],
        }),
        //1 Set
        StructFunc::Struct(BuiltInStruct{
            name: "HashSet",
            generics: Some(vec!["T"]),
            methods: vec![ // todo
               "__init__(self)",
                "add(t: T)",
            ],
            _parameters: vec![],
        }),
        //1 Dict
        StructFunc::Struct(BuiltInStruct{
            name: "HashMap",
            generics: Some(vec!["K", "V"]),
            methods: vec![
                "__init__(self)",
            ], // todo
            _parameters: vec![],
        }),
        //2 __len__
        StructFunc::Trait(BuiltInTrait {
            name: "__len__",
            duck: true,
            generics: None,
            methods: vec![
                "__len__(self) -> int"
            ],
            types: None,
            _parameters: vec![],
            ignore: false
        }),
        //4 len
        StructFunc::Func(BuiltInFunc {
            name: "len",
            generics: None,
            args: vec!["x: __len__"],
            return_typ: Some("int"),
        }),
        //2 __next__
        StructFunc::Trait(BuiltInTrait {
            name: "__next__",
            duck: true,
            generics: None,
            methods: vec![
                "__next__(self: &mut Self) -> Inner"
            ],
            types: Some(vec!["Inner"]),
            _parameters: vec![],
            ignore: false
        }),

        // // 4 range
        // StructFunc::Func(BuiltInFunc {
        //     name: "range",
        //     generics: None,
        //     args: vec!["start: int", "end: int", "step: int"],
        //     return_typ: Some("_Iterator_[int]"),
        // }),
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
            StructFunc::Trait(trt) => {
                if trt.ignore {
                    unsafe {
                        IGNORE_TRAITS.insert(trt.name);
                    }
                }
                if trt.duck {
                    write!(data, "trait {}", trt.name).unwrap();
                } else {
                    write!(data, "TRAIT {}", trt.name).unwrap();
                }
                if let Some(generics) = trt.generics {
                    write!(data, "<{}>", join(generics.iter(), ",")).unwrap();
                }
                write!(data, ":").unwrap();
                if let Some(typs) = trt.types {
                    for typ in typs {
                        write!(data, "\n\ttype {typ}").unwrap();
                    }
                }
                for func in trt.methods {
                    write!(data, "\n\tdef {}", func).unwrap();
                }
                writeln!(data).unwrap();
            }
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
    write!(data, "\n{input}").unwrap();
    // println!("{data}");
    data
}



pub fn make_built_ins() -> HashMap<&'static str, Box<dyn BuiltIn>> {
    let mut res: HashMap<&'static str, Box<dyn BuiltIn>> = HashMap::new();

    let int_type = typ_with_child! {
        INT_TYPE,
        Type {
            kind: TypeKind::GenericsMap,
            children: None
        }
    };
    let mut_str_type = typ_with_child! {
        MUT_STR_TYPE,
        Type {
            kind: TypeKind::GenericsMap,
            children: None
        }
    };
    let str_type = typ_with_child! {
        STR_TYPE,
        Type {
            kind: TypeKind::GenericsMap,
            children: None
        }
    };
    let bool_type = typ_with_child! {
        BOOL_TYPE,
        Type {
            kind: TypeKind::GenericsMap,
            children: None
        }
    };
    let char_type = typ_with_child! {
        CHAR_TYPE,
        Type {
            kind: TypeKind::GenericsMap,
            children: None
        }
    };
    let float_type = typ_with_child! {
        FLOAT_TYPE,
        Type {
            kind: TypeKind::GenericsMap,
            children: None
        }
    };
    let generic_iter = typ_with_child! {
        ITER_TYPE,
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
    // /*1 range */{
    //     res.insert(
    //         "range",
    //         Box::new(Range {
    //             input_types: some_vec![
    //                 int_type.clone(),
    //                 Type {
    //                     kind: TypeKind::Optional,
    //                     children: some_vec![ int_type.clone(), int_type.clone() ],
    //                 }
    //             ],
    //             output_types: Some(Type {
    //                 kind: ITER_TYPE,
    //                 children: some_vec![
    //                     Type {
    //                         kind: TypeKind::GenericsMap,
    //                         children: some_vec![
    //                             Type {
    //                                 kind: TypeKind::Generic(GenericType::Of(String::from("T"))),
    //                                 children: some_vec![int_type.clone()]
    //                             }
    //                         ]
    //                     }],
    //             }),
    //             for_strct: None,
    //         }),
    //     );
    // }
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
                    kind: ITER_TYPE,
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
                        kind: TypeKind::Trait(TypName::Static("Display")),
                        children: None
                    }
                ],
                output_types: Some(mut_str_type.clone()),
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
                        children: some_vec![mut_str_type.clone(), str_type.clone(), char_type.clone(), bool_type.clone(), float_type.clone()]
                    }
                ],
                output_types: Some(int_type.clone()),
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
                        children: some_vec![
                            mut_str_type.clone(), str_type.clone(), char_type.clone(),
                            bool_type.clone(), int_type.clone()
                        ]
                    }
                ],
                output_types: Some(float_type.clone()),
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    );
    fn to_str_python(
        &self, ast: &[Ast], children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>) -> String;
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        write!(
            res,
            "println!(\"{}\", ",
            "{} "
                .repeat(unwrap_u(&ast[children[1]].children).len())
                .strip_suffix(' ')
                .unwrap_or("")
        )
        .unwrap();
        close_rust(ast, res, children, built_ins, enums);
    }
    to_py!("print({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        write!(
            res,
            "println!(\"{}\", ",
            "{:?} "
                .repeat(unwrap_u(&ast[children[1]].children).len())
                .strip_suffix(' ')
                .unwrap()
        )
        .unwrap();
        close_rust(ast, res, children, built_ins, enums);
    }

    to_py!("print({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
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

    to_py!("range({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".rev()").unwrap();
    }

    to_py!("reversed({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".enumerate()").unwrap();
    }

    to_py!("enumerate({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".to_string()").unwrap();
    }

    to_py!("str({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".parse::<i32>().unwrap()").unwrap();
    }

    to_py!("int({}");
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
        ast: &[Ast],
        res: &mut String,
        children: &[usize],
        built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
        enums: &mut HashMap<String, String>,
    ) {
        to_rust(ast, children[1], 0, res, built_ins, enums);
        write!(res, ".parse::<f32>().unwrap()").unwrap();
    }

    to_py!("float({}");
}


fn close_python(
    ast: &[Ast], children: &[usize],
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>, add_index: bool
) -> String {
    if children.len() > 1 {
        format!("{})", to_python(ast, children[1], 0, built_ins, ToWrapVal::GetAsValue))
    } else {
        String::from(")")
    }
}
fn close_rust(
    ast: &[Ast],
    res: &mut String,
    children: &[usize],
    built_ins: &HashMap<&str, Box<dyn BuiltIn>>,
    enums: &mut HashMap<String, String>,
) {
    if children.len() > 1 {
        to_rust(ast, children[1], 0, res, built_ins, enums);
    }
    write!(res, ")").unwrap();
}
