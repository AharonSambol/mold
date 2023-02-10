use crate::construct_ast::ast_structure::join;
use std::fmt::Write;
use crate::{EMPTY_STR, IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, make_primitive};


enum BuiltIn { Struct(BuiltInStruct), Func(BuiltInFunc), Trait(BuiltInTrait), Enum(BuiltInEnum) }
struct BuiltInStruct {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    methods: Vec<&'static str>,
    types: Option<Vec<&'static str>>,
    traits: Option<Vec<&'static str>>,
}
struct BuiltInTrait {
    name: &'static str,
    duck: bool,
    generics: Option<Vec<&'static str>>,
    methods: Vec<&'static str>,
    types: Option<Vec<&'static str>>,
    ignore: bool
}
struct BuiltInFunc {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    args: Vec<&'static str>,
    return_typ: Option<&'static str>
}
struct BuiltInEnum {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    args: Vec<&'static str>,
    ignore: bool
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
        BuiltIn::Struct(BuiltInStruct{
            name: "String",
            generics: None,
            types: None,
            methods: vec![
                "clone(self) -> str", // todo do automatically?
                "split(self, s: str) -> List[str]", //todo (optional) s: str | char    if rust: -> Iter[str]
                "strip(self) -> str",        //todo (optional) c: char
                "lstrip(self) -> str",        //todo (optional) c: char
                "rstrip(self) -> str",        //todo (optional) c: char
                "len(self) -> int",         //todo -> usize technically
                "contains(self, s: str) -> bool", //todo s: str | char
                "replace(self, orig: str, new: str) -> str", //todo orig/new: str | char
                "startswith(self, s: str) -> bool",
                "endswith(self, s: str) -> bool",
                "find(self, s: str) -> int",  //todo s: str | char
                "count(self, s: str) -> int",  //todo s: str | char
                "removeprefix(self, s: str) -> str",
                "removesuffix(self, s: str) -> str",
                "lower(self, s: str) -> str",
                "upper(self, s: str) -> str",
                "__init__(self)",
                "__str__(self: &Self) -> str",
                // todo chars()
                // todo is(digit\numeric\ascii...)
                // todo "join(lst: List[T]) -> int",
            ],
            traits: Some(vec!["Debug", "Display"]),
        }),
        // Iter / IterMut
        /*//1 Iter
        BuiltIn::Struct(BuiltInStruct{
            name: "Iter",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "into_iter(self) -> Iterator[Item=&T]",
                "next(self: &mut Self) -> Option[&T]",
            ],
            types: Some(vec![
                "IntoIterator.Item = &T",
                "Iterator.Item = &T"
            ]),
            traits: Some(vec!["Debug"]),
        }),
        //1 IterMut
        BuiltIn::Struct(BuiltInStruct{
            name: "IterMut",
            generics: Some(vec!["T"]),
            methods: vec![
                "__init__(self)",
                "into_iter(self) -> Iterator[Item=&mut T]",
                "next(self: &mut Self) -> Option[&mut T]"
            ],
            types: Some(vec![
                "IntoIterator.Item = &mut T",
                "Iterator.Item = &mut T"
            ]),
            traits: Some(vec!["Debug"]),
        }),*/
        //1 Box
        BuiltIn::Struct(BuiltInStruct{
            name: "Box",
            generics: Some(vec!["T"]),
            types: None,
            methods: vec![
                "__init__(self)",
                "new(t: T) -> Box[T]",
            ],
            traits: Some(vec!["Debug"]),
        }),
        //1 Vec
        BuiltIn::Struct(BuiltInStruct{
            name: "Vec",
            generics: Some(vec!["T"]),
            types: Some(vec![
                "IntoIterator.Item = T"
            ]),
            methods: vec![
                "__len__(self: &Self) -> int",
                "__init__(self)",
                "__str__(self: &Self) -> str",
                "into_iter(self) -> IntoIterator[Item=T]",
                "append(self, t: T)",
                "index(self, pos: usize) -> T",
            ],
            traits: Some(vec!["Debug"]),
        }),
        //1 HashSet
        BuiltIn::Struct(BuiltInStruct{
            name: "HashSet",
            generics: Some(vec!["T"]),
            types: Some(vec![
                "IntoIterator.Item = T"
            ]),
            methods: vec![ // todo
                "__len__(self: &Self) -> int",
                "__init__(self)",
                "__str__(self: &Self) -> str",
                "add(self, t: T)",
                "into_iter(self) -> IntoIterator[Item=T]",
            ],
            traits: Some(vec!["Debug"]),
        }),
        //1 HashMap
        BuiltIn::Struct(BuiltInStruct{
            name: "HashMap",
            generics: Some(vec!["K", "V"]),
            types: Some(vec![
                "IntoIterator.Item = K"
            ]),
            methods: vec![
                "__init__(self)",
                "__len__(self: &Self) -> int",
                "__str__(self: &Self) -> str",
                "into_iter(self) -> IntoIterator[Item=K]",
            ], // todo
            traits: Some(vec!["Debug"]),
        }),
        //2 __len__
        BuiltIn::Trait(BuiltInTrait {
            name: "__len__",
            duck: true,
            generics: None,
            methods: vec![
                "__len__(self: &Self) -> int"
            ],
            types: None,
            ignore: false
        }), // todo Sized?
        //2 __str__
        BuiltIn::Trait(BuiltInTrait {
            name: "__str__",
            duck: true,
            generics: None,
            methods: vec![
                "__str__(self: &Self) -> str",
            ],
            types: None,
            ignore: false
        }),
        //2 Iterator
        BuiltIn::Trait(BuiltInTrait {
            name: "Iterator",
            duck: false,
            generics: None,
            methods: vec![
                "into_iter(self) -> IntoIterator[Item=Item]",
                "next(self: &mut Self) -> Option[Item]"
            ],
            types: Some(vec!["Item"]),
            ignore: true
        }),
        //2 Display
        BuiltIn::Trait(BuiltInTrait {
            name: "Display",
            duck: false,
            generics: None,
            methods: vec![],
            types: None,
            ignore: true,
        }),
        //2 Debug
        BuiltIn::Trait(BuiltInTrait {
            name: "Debug",
            duck: false,
            generics: None,
            methods: vec![],
            types: None,
            ignore: true,
        }),
        //2 IntoIterator
        BuiltIn::Trait(BuiltInTrait{
            name: "IntoIterator",
            duck: false,
            generics: None,
            methods: vec![
                "into_iter(self) -> Iterator[Item=Item]"
            ],
            types: Some(vec!["Item"]),
            ignore: true,
        }),
        //3 Option
        BuiltIn::Enum(BuiltInEnum {
            name: "Option",
            generics: Some(vec!["T"]),
            args: vec!["Some(T)", "None"],
            ignore: true,
        }),
        //4 len
        BuiltIn::Func(BuiltInFunc {
            name: "len",
            generics: None,
            args: vec!["x: &__len__"],
            return_typ: Some("int"),
        }),
        //4 min TODO add support for lambda
        BuiltIn::Func(BuiltInFunc {
            name: "min",
            generics: Some(vec!["T"]),
            args: vec!["x: IntoIterator[Item=T]"],
            return_typ: Some("T"),
        }),
        //4 max TODO add support for lambda
        BuiltIn::Func(BuiltInFunc {
            name: "max",
            generics: Some(vec!["T"]),
            args: vec!["x: IntoIterator[Item=T] | Iterator[Item=T]"],
            return_typ: Some("T"),
        }),
        //4 sum
        BuiltIn::Func(BuiltInFunc {
            name: "sum",
            generics: Some(vec!["T"]),
            args: vec!["x: IntoIterator[Item=T]"],
            return_typ: Some("T"),
        }),
        //4 abs
        BuiltIn::Func(BuiltInFunc {
            name: "abs",
            generics: Some(vec!["T"]), // todo which i8 | i16 | i32 | i64 | i128 | isize | f32 | f64
            args: vec!["x: T"],
            return_typ: Some("T"),
        }),
        //4 pow
        BuiltIn::Func(BuiltInFunc {
            name: "pow",
            generics: Some(vec!["T, G"]), // todo which i8 | i16 | i32 | i64 | i128 | isize | f32 | f64
            args: vec!["base: T, pow: G, mod: G = 0"], //1 base, pow, mod?
            return_typ: Some("T"),
        }),
        //4 range
        BuiltIn::Func(BuiltInFunc {
            name: "range",
            generics: None,
            args: vec!["start: int", "end: int | bool = False", "step: int | bool = False"],
            // args: vec!["*args: int"], //1 so that the rust version can count how many args were supplied
            return_typ: Some("Iterator[Item=int]"),
        }),
        //4 print
        BuiltIn::Func(BuiltInFunc {
            name: "print",
            generics: None,
            args: vec!["*a: __str__"], //1 the type (Display) is ignored, anything is accepted
            return_typ: None,
        }),
        //4 reversed
        BuiltIn::Func(BuiltInFunc{
            name: "reversed",
            generics: Some(vec!["T"]),
            args: vec!["t: Iterator[Item=T]"],
            return_typ: Some("Iterator[Item=T]"),
        }),
        //4 iter
        BuiltIn::Func(BuiltInFunc{
            name: "iter",
            generics: Some(vec!["T"]),
            args: vec!["t: IntoIterator[Item=T]"],
            return_typ: Some("Iterator[Item=&mut T]"),
        }),
        //4 iter imut
        BuiltIn::Func(BuiltInFunc{
            name: "iter_imut",
            generics: Some(vec!["T"]),
            args: vec!["t: IntoIterator[Item=T]"],
            return_typ: Some("Iterator[Item=&T]"),
        }),
        /* //1 Rev
        StructFunc::Struct(BuiltInStruct{
            name: "Rev",
            generics: Some(vec!["T"]), // this should be Iter[T]
            methods: vec![
                "into_iter() -> IntoIter[T::Item]", //3 this is what's wrong
                "iter() -> Iter[T]",
            ],
        }),

         */
    ];
    writeln!(data).unwrap();
    for add in to_add {
        match add {
            BuiltIn::Trait(trt) => {
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
            BuiltIn::Struct(stct) => {
                unsafe {
                    IGNORE_STRUCTS.insert(stct.name);
                }
                if let Some(generics) = stct.generics {
                    write!(data, "struct {}<{}>", stct.name, join(generics.iter(), ",")).unwrap();
                } else {
                    write!(data, "struct {}", stct.name).unwrap();
                }
                if let Some(traits) = stct.traits {
                    write!(data, "({}):", join(traits.iter(), ",")).unwrap();
                } else {
                    write!(data, ":").unwrap();
                }
                if let Some(types) = stct.types {
                    for typ in types {
                        write!(data, "\n\ttype {typ}").unwrap();
                    }
                }
                for func in stct.methods {
                    write!(data, "\n\tdef {func}:").unwrap();
                }
                writeln!(data).unwrap();
            },
            BuiltIn::Func(func) => {
                unsafe {
                    IGNORE_FUNCS.insert(func.name); // if you make this optional you need to also change where I don't box vals passed to builtin funcs
                }
                let args = join(func.args.iter(), ",");
                let rtrn = if let Some(t) = func.return_typ {
                    format!("-> {t}")
                } else {
                    EMPTY_STR
                };
                let generics = if let Some(generics) = func.generics {
                    format!("<{}>", join(generics.iter(), ","))
                } else {
                    EMPTY_STR
                };
                writeln!(data, "def {}{generics}({args}){rtrn}:\n\tpass", func.name).unwrap();
            },
            BuiltIn::Enum(enm) => {
                if enm.ignore {
                    unsafe { IGNORE_ENUMS.insert(enm.name); }
                }
                let args = join(enm.args.iter(), "\n\t");
                let generics = if let Some(generics) = enm.generics {
                    format!("<{}>", join(generics.iter(), ","))
                } else {
                    EMPTY_STR
                };
                writeln!(data, "enum {}{generics}:\n\t{args}", enm.name).unwrap();
            }
        }
    }
    write!(data, "\n{input}").unwrap();
    // println!("{data}");
    data
}


/*
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
*/