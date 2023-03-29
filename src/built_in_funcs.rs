use std::fmt::Write;
use crate::{EMPTY_STR, IGNORE_ENUMS, IGNORE_FUNCS, IGNORE_STRUCTS, IGNORE_TRAITS, make_primitive};


enum BuiltIn { Struct(BuiltInStruct), Func(BuiltInFunc), Trait(BuiltInTrait), Enum(BuiltInEnum) }
struct BuiltInStruct {
    name: &'static str,
    generics: Option<Vec<&'static str>>,
    methods: Vec<&'static str>,
    methods_with_impl: Vec<&'static str>,
    // types: Option<Vec<&'static str>>,
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
            methods: vec![
                "fmt(self: &Self, f: &mut Formatter['_]) -> Result[(), Error]",
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
                // todo chars()
                // todo is(digit\numeric\ascii...)
                // todo "join(lst: List[T]) -> int",
            ],
            methods_with_impl: vec![],
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
            methods: vec![
                "new(t: T) -> Box[T]",
            ],
            methods_with_impl: vec!["def __init__(self, x: T):\n\t\tself.x = x"],
            traits: Some(vec!["Debug"]),
        }),
        //1 Vec
        BuiltIn::Struct(BuiltInStruct{
            name: "Vec",
            generics: Some(vec!["T"]),
            methods: vec![
                "__len__(self: &Self) -> int",
                "into_iter(self) -> IntoIterator[Item=T]",
                "append(self, t: T)",
                "index(self, pos: usize) -> T",
                "Debug::fmt(self: &Self, f: &mut Formatter['_]) -> Result[(), Error]",
            ],
            methods_with_impl: vec!["def __init__(self, x: T):\n\t\tself.x = x"],
            traits: Some(vec!["Debug", "IntoIterator[Item=T]"]),
        }),
        //1 HashSet
        BuiltIn::Struct(BuiltInStruct{
            name: "HashSet",
            generics: Some(vec!["T"]),
            methods: vec![ // todo
                "__len__(self: &Self) -> int",
                "add(self, t: T)",
                "into_iter(self) -> IntoIterator[Item=T]",
            ],
            methods_with_impl: vec!["def __init__(self, x: T):\n\t\tself.x = x"],
            traits: Some(vec!["Debug", "IntoIterator[Item=T]"]),
        }),
        //1 HashMap
        BuiltIn::Struct(BuiltInStruct{
            name: "HashMap",
            generics: Some(vec!["K", "V"]),
            methods: vec![
                "__len__(self: &Self) -> int",
                "into_iter(self) -> IntoIterator[Item=K]",
            ], // todo
            methods_with_impl: vec!["def __init__(self, k: K, v: V):\n\t\tself.k = k\n\t\tself.v = v"],
            traits: Some(vec!["Debug", "IntoIterator[Item=K]"]),
        }),
        //1 Formatter
        BuiltIn::Struct(BuiltInStruct{
            name: "Formatter",
            generics: Some(vec!["'_"]),
            methods: vec![],
            methods_with_impl: vec!["def __init__(self, x: '_):\n\t\tself.x = x"],
            traits: None,
        }),
        //1 Error
        BuiltIn::Struct(BuiltInStruct{
            name: "Error",
            generics: None,
            methods: vec!["__init__(self)"],
            methods_with_impl: vec![],
            traits: None,
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
        //2 Iterator
        BuiltIn::Trait(BuiltInTrait {
            name: "Iterator",
            duck: false,
            generics: None,
            methods: vec![
                "into_iter(self) -> IntoIterator[Item=Item]",
                "next(self: &mut Self) -> Option[Item]"
            ],
            types: Some(vec!["Item=None"]),
            ignore: true
        }),
        //2 Display
        BuiltIn::Trait(BuiltInTrait {
            name: "Display",
            duck: false,
            generics: None,
            methods: vec![
                "fmt(self: &Self, f: &mut Formatter['_]) -> Result[(), Error]"
            ],
            types: None,
            ignore: true,
        }),
        //2 Debug
        BuiltIn::Trait(BuiltInTrait {
            name: "Debug",
            duck: true,
            generics: None,
            methods: vec![
                "fmt(self: &Self, f: &mut Formatter['_]) -> Result[(), Error]"
            ],
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
            types: Some(vec!["Item=None"]),
            ignore: true,
        }),
        //3 Option
        BuiltIn::Enum(BuiltInEnum {
            name: "Option",
            generics: Some(vec!["T"]),
            args: vec!["Some(T)", "None"],
            ignore: true,
        }),
        //3 Result
        BuiltIn::Enum(BuiltInEnum {
            name: "Result",
            generics: Some(vec!["T", "E"]),
            args: vec!["Ok(T)", "Err(E)"],
            ignore: true,
        }),
        //4 len TODO does this work??
        BuiltIn::Func(BuiltInFunc {
            name: "len",
            generics: None,
            args: vec!["x: &__len__"],
            return_typ: Some("int"),
        }),
        // what if you can pass it a __len__ obj and itll (cast to trait) and add the pointer automatically
        //4 min TODO add support for lambda and for min(1, 2)
        BuiltIn::Func(BuiltInFunc {
            name: "min",
            generics: Some(vec!["T"]),
            args: vec!["x: IntoIterator[Item=T]"],
            return_typ: Some("T"),
        }),
        //4 max TODO add support for lambda and for max(1, 2)
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
            args: vec!["base: T, pow: G, mod: G | None = None"], //1 base, pow, mod?
            return_typ: Some("T"),
        }),
        //4 range
        BuiltIn::Func(BuiltInFunc {
            name: "range",
            generics: None,
            args: vec!["start: int", "end: int | None = None", "step: int | None = None"],
            // args: vec!["*args: int"], //1 so that the rust version can count how many args were supplied
            return_typ: Some("Iterator[Item=int]"),
        }),
        //4 print
        BuiltIn::Func(BuiltInFunc {
            name: "print",
            generics: None,
            args: vec!["*a: Display | Debug"], //1 the type (Display) is ignored, anything is accepted
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
        //4 clone
        BuiltIn::Func(BuiltInFunc{
            name: "clone",
            generics: Some(vec!["T"]),
            args: vec!["t: &T"],
            return_typ: Some("T"),
        }),
        //4 input
        BuiltIn::Func(BuiltInFunc{
            name: "input",
            generics: None,
            args: vec!["x: str | None = None"],
            return_typ: Some("str"),
        }),
        //4 int
        BuiltIn::Func(BuiltInFunc{
            name: "int",
            generics: None,
            args: vec!["x: &str | float | bool, base: int | None = None"],
            return_typ: Some("int"),
        }),
        //4 str
        BuiltIn::Func(BuiltInFunc{
            name: "str",
            generics: None,
            args: vec!["x: int | float | bool"],
            return_typ: Some("str"),
        }),
        //4 exit
        BuiltIn::Func(BuiltInFunc{
            name: "exit",
            generics: None,
            args: vec!["code: int = 0"],
            return_typ: None,
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
                    if let Some(typs) = trt.types {
                        write!(data, "[{}, {}]", generics.join(","), typs.join(",")).unwrap();
                    } else {
                        write!(data, "[{}]", generics.join(",")).unwrap();
                    }
                } else if let Some(typs) = trt.types {
                    write!(data, "[{}]", typs.join(",")).unwrap();
                }
                write!(data, ":").unwrap();
                // if let Some(typs) = trt.types {
                //     for typ in typs {
                //         write!(data, "\n\ttype {typ}").unwrap();
                //     }
                // }
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
                    write!(data, "struct {}[{}]", stct.name, generics.join(",")).unwrap();
                } else {
                    write!(data, "struct {}", stct.name).unwrap();
                }
                if let Some(traits) = stct.traits {
                    write!(data, "({}):", traits.join(",")).unwrap();
                } else {
                    write!(data, ":").unwrap();
                }
                for func in stct.methods {
                    write!(data, "\n\tdef {func}:").unwrap();
                }
                for func in stct.methods_with_impl {
                    write!(data, "\n\t{func}").unwrap();
                }
                writeln!(data).unwrap();
            },
            BuiltIn::Func(func) => {
                unsafe {
                    IGNORE_FUNCS.insert(func.name); // if you make this optional you need to also change where I don't box vals passed to builtin funcs
                }
                let args = func.args.join(",");
                let rtrn = if let Some(t) = func.return_typ {
                    format!("-> {t}")
                } else {
                    EMPTY_STR
                };
                let generics = if let Some(generics) = func.generics {
                    format!("[{}]", generics.join(","))
                } else {
                    EMPTY_STR
                };
                writeln!(data, "def {}{generics}({args}){rtrn}:\n\tpass", func.name).unwrap();
            },
            BuiltIn::Enum(enm) => {
                if enm.ignore {
                    unsafe { IGNORE_ENUMS.insert(enm.name); }
                }
                let args = enm.args.join("\n\t");
                let generics = if let Some(generics) = enm.generics {
                    format!("[{}]", generics.join(","))
                } else {
                    EMPTY_STR
                };
                writeln!(data, "enum {}{generics}:\n\t{args}", enm.name).unwrap();
            }
        }
    }
    write!(data, "\n{input}").unwrap();
    data
}

