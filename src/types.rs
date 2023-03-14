use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use pretty_print_tree::{Color, PrettyPrintTree};
use crate::construct_ast::ast_structure::{Ast, join, Param};
use crate::{EMPTY_STR, get_traits, IMPL_TRAITS, ImplTraitsKey, ImplTraitsVal};
use crate::add_types::polymorphism::escape_typ_chars;
use crate::add_types::utils::get_pointer_complete_inner;
use crate::construct_ast::mold_ast::{add_trait_to_struct, get_trt_strct_functions, Info, TraitFuncs};

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};
const EMPTY_PARAM: Param = Param {
    typ: UNKNOWN_TYPE, name: EMPTY_STR, is_mut: false, is_args: false, is_kwargs: false, pos: usize::MAX
};
pub const STR_TYPE: TypeKind = TypeKind::Struct(TypName::Static("str"));
pub const MUT_STR_TYPE: TypeKind = TypeKind::Struct(TypName::Static("String"));
pub const BOOL_TYPE: TypeKind = TypeKind::Struct(TypName::Static("bool"));
pub const INT_TYPE: TypeKind = TypeKind::Struct(TypName::Static("i32"));
pub const FLOAT_TYPE: TypeKind = TypeKind::Struct(TypName::Static("f32"));
pub const CHAR_TYPE: TypeKind = TypeKind::Struct(TypName::Static("char"));
// pub const ITER_TYPE: Type = Type {
//     kind: TypeKind::Struct(ITER_NAME),
//     children: None
// };

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericType {
    Declaration(String),
    // Of(String),
    NoVal(String),
    WithVal(String)
}

#[derive(Debug, Clone)]
pub enum TypName {
    Str(String),
    Static(&'static str)
}

impl PartialEq for TypName {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.get_str() == other.get_str() }
}
impl PartialEq<&'static str> for TypName {
    #[inline]
    fn eq(&self, other: &&'static str) -> bool { self.get_str() == *other }
}
impl PartialEq<str> for TypName {
    #[inline]
    fn eq(&self, other: &str) -> bool { self.get_str() == other }
}
impl PartialEq<String> for TypName {
    #[inline]
    fn eq(&self, other: &String) -> bool { self.get_str() == *other }
}

impl TypName {
    #[inline]
    pub fn get_str(&self) -> &str {
        match self {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    EmptyType,
    Generic(GenericType),
    Generics,
    GenericsMap,
    OneOf,
    _Tuple,
    InnerType(String), // e.g. Iterator[Inner=i32]
    _Args,
    Trait(TypName),
    Enum(TypName),
    Unknown,
    Function(String),
    Struct(TypName), // child[0] = generics
    _Class(String),
    Pointer,
    MutPointer,
    Null,
    Tuple,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub children: Option<Vec<Type>>
}

impl Eq for Type {}
impl PartialEq for Type {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.children == other.children
    }
}
impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

#[allow(dead_code)]
#[track_caller]
pub fn print_type(typ: &Option<Type>) {
    let caller_location = std::panic::Location::caller();
    let caller_file = caller_location.file();
    let caller_line_number = caller_location.line();
    println!("[{caller_file}:{caller_line_number}]");

    let ppt: PrettyPrintTree<Type> = {
        PrettyPrintTree::<Type>::new(
            Box::new(|typ| {
                format!("{:?}", typ.kind)
            }),
            Box::new(|typ| {
                unwrap(&typ.children).clone()
            }),
        )
    };
    if let Some(t) = typ {
        println!("{}", ppt.to_str(t));
    } else {
        println!("None");
    }
    println!("\n");
}

#[allow(dead_code)]
#[track_caller]
pub fn print_type_b(typ: &Option<Type>, color: Color){
    let caller_location = std::panic::Location::caller();
    let caller_file = caller_location.file();
    let caller_line_number = caller_location.line();
    println!("[{caller_file}:{caller_line_number}]");

    let mut ppt: PrettyPrintTree<Type> = {
        PrettyPrintTree::<Type>::new(
            Box::new(|typ| {
                format!("{:?}", typ.kind)
            }),
            Box::new(|typ| {
                unwrap(&typ.children).clone()
            }),
        )
    };
    ppt.color = color;
    if let Some(t) = typ {
        println!("{}", ppt.to_str(t));
    } else {
        println!("None");
    }
    println!("\n");
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Tuple => {
                write!(f, "({})", join(unwrap(&self.children).iter(), ", "))
            },
            TypeKind::Null => write!(f, "None"),
            TypeKind::EmptyType => write!(f, "()"),
            TypeKind::Unknown => write!(f, "UNKNOWN TYPE"),
            TypeKind::OneOf => {
                let children = unwrap(&self.children);
                let mut children: Vec<_> = children.iter()
                    .map(|x| escape_typ_chars(&x.to_string())).collect();
                children.sort();
                write!(f, "{}", children.join("__or__"))
            },
            TypeKind::_Tuple => {
                write!(f, "({})", join(unwrap(&self.children).iter(), ","))
            },
            TypeKind::Generic(c) => {
                match c {
                    GenericType::WithVal(_) =>
                        write!(f, "{}", self.ref_children()[0]),
                    GenericType::NoVal(name) => write!(f, "{name}"),
                    GenericType::Declaration(_) => write!(f, "GENERIC({c:?})")
                }
            },
            TypeKind::GenericsMap => write!(
                f, "GENERICS_MAP({})", join(unwrap(&self.children).iter(), ",")
            ),
            TypeKind::Generics => write!(
                f, "GENERICS({})", join(unwrap(&self.children).iter(), ",")
            ),
            TypeKind::_Args => {
                write!(f, "ARGS({})", unwrap(&self.children)[0])
            },
            TypeKind::Trait(name) => {
                let gens = self.format_generics();
                write!(f, "Box<dyn {name}{gens}>")
            },
            TypeKind::Enum(name) => {
                let gens = self.format_generics();
                write!(f, "{name}{gens}")
            },
            TypeKind::Struct(name) => {
                let gens = self.format_generics();
                write!(f, "{name}{gens}")
            },
            TypeKind::Function(name) => write!(f, "{name}"),
            TypeKind::_Class(_) => {
                todo!()
            }
            TypeKind::Pointer => write!(f, "&{}", unwrap(&self.children)[0]),
            TypeKind::MutPointer => write!(f, "&mut {}", unwrap(&self.children)[0]),
            TypeKind::InnerType(name) => {
                if self.children.is_none() {
                    write!(f, "Self::{name}")
                } else {
                    write!(f, "{name}={}", unwrap(&self.children)[0])
                }
            },
        }
    }
}

impl Type {
    // TODO dont have same one twice e.g. int | bool + bool | str != int | int | ... (BUT reserve who is first (for pointer))
    pub fn add_option(mut self, mut typ: Type) -> Type {
        if let TypeKind::OneOf = self.kind {
            if let TypeKind::OneOf = typ.kind {
                if let Some(vc) = &mut self.children {
                    vc.append(typ.children.as_mut().unwrap());
                } else {
                    panic!("adding two empty 'OneOf' types together??")
                }
            } else if let Some(vc) = &mut self.children {
                vc.push(typ);
            } else { panic!("an empty 'OneOf' type?") }
            self
        } else if let TypeKind::OneOf = typ.kind {
            if let Some(vc) = &mut typ.children {
                vc.insert(0, self)
            } else { panic!("an empty 'OneOf' type?") }
            typ
        } else {
            Type {
                kind: TypeKind::OneOf,
                children: Some(vec![self, typ])
            }
        }
    }

    pub fn contains(&self, other: &Self) -> bool {
        let (TypeKind::OneOf, TypeKind::OneOf) = (&self.kind, &other.kind) else {
            unreachable!()
        };
        let self_children = self.ref_children();
        let other_children = other.ref_children();
        if self_children.len() < other_children.len() { return false }
        other_children.iter().all(|opt|
            self_children.iter().any(|s_opt| opt == s_opt)
        ) // 3 NOT EFFICIENT
    }

    fn format_generics(&self) -> String {
        if let Some(children) = &self.children {
            if !children.is_empty() {
                if let TypeKind::GenericsMap = children[0].kind {
                    if let Some(generics) = &children[0].children {
                        return format!("::<{}>", join(generics.iter(), ","));
                    }
                } else if let Some(generics) = &children[0].children {
                    return format!("<{}>", join(generics.iter(), ","));
                }
            }
        }
        EMPTY_STR
    }
    #[inline]
    pub fn ref_children(&self) -> &Vec<Type> {
        self.children.as_ref().unwrap()
    }
}

impl Display for TypName {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            TypName::Str(s) => s.as_str(),
            TypName::Static(s) => s
        })
    }
}


static EMPTY_VEC: Vec<Type> = vec![];
#[inline]
pub fn unwrap(children: &Option<Vec<Type>>) -> &Vec<Type> {
    if let Some(c) = &children { c } else { &EMPTY_VEC }
}
static EMPTY_VEC_US: Vec<usize> = vec![];
#[inline]
pub fn unwrap_u(children: &Option<Vec<usize>>) -> &Vec<usize> {
    if let Some(c) = &children { c } else { &EMPTY_VEC_US }
}

pub fn clean_type(st: String) -> TypName {
    TypName::Static(
        match st.as_str() {
            "str" => "String",
            "int" => "i32",
            "float" => "f32",
            "List" | "list" => "Vec",
            "Set" => "HashSet",
            "Dict" => "HashMap",
            _ => return TypName::Str(st)
        }
    )
}

pub fn implements_trait(
    mut typ: &Type, expected_trait: &str, ast: &[Ast], info: &Info
) -> bool { //, Option<HashMap<String, Type>>) {
    #[inline] fn struct_matches_trait(trt_funcs: &TraitFuncs, funcs: &TraitFuncs, trait_name: &str) -> Option<HashMap<String, Type>> {
        let mut hm = HashMap::new();
        'trait_func_loop: for (trt_f_name, (_, trt_f_types)) in trt_funcs {
            let mut func_types = funcs.get(trt_f_name);
            if func_types.is_none() {
                func_types = funcs.get(&format!("{trait_name}::{trt_f_name}"));
            }
            if let Some((_, func_types)) = func_types {
                if func_types == trt_f_types {
                    continue 'trait_func_loop
                }
                if func_types.output.is_some() != trt_f_types.output.is_some()
                    || func_types.input.is_some() != trt_f_types.input.is_some()
                { return None }
                let mut func_all_types =
                    if let Some(v) = &func_types.input { v.clone() }
                    else { vec![] };
                let mut trt_f_all_types =
                    if let Some(v) = &trt_f_types.input { v.clone() }
                    else { vec![] };
                if let Some(x) = &func_types.output {
                    func_all_types.push(Param {
                        typ: x.clone(), ..EMPTY_PARAM
                    });
                    unsafe { //1 safe cuz already checked that they both have or both dont have a return typ
                        trt_f_all_types.push(Param {
                            typ: trt_f_types.output.clone().unwrap_unchecked(),
                            ..EMPTY_PARAM
                        })
                    }
                }

                for (trt_fnc, fnc) in trt_f_all_types.iter().zip(func_all_types) {
                    if trt_fnc.name == fnc.name && trt_fnc.typ == fnc.typ {
                        continue
                    }
                    if let TypeKind::InnerType(typ_name) = &trt_fnc.typ.kind {
                        if let Some(expected_typ) = hm.get(typ_name) {
                            if fnc.typ != *expected_typ {
                                return None
                            }
                        } else {
                            hm.insert(typ_name.clone(), fnc.typ);
                        }
                    } else {
                        return None
                    }
                }
                continue 'trait_func_loop
            }
            return None
        }
        Some(hm)
    }

    if let TypeKind::Generic(GenericType::WithVal(_)) = &typ.kind {
        typ = &unwrap(&typ.children)[0];
    }
    if let TypeKind::Pointer | TypeKind::MutPointer = &typ.kind {
        if expected_trait == "Debug" { // TODO this probably has to do with dereferencing or smthing
            typ = get_pointer_complete_inner(typ);
        }
    }
    if let TypeKind::OneOf = &typ.kind {
        let res = typ.ref_children().iter().all(
            |typ| implements_trait(typ, expected_trait, ast, info)
        );
        if !res {
            return false
        }

        // TODO when implementing Display for a one_of_enum needs an actual fn impl
        // TODO ignore some implementations (IGNORE_STRUCT/ IGNORE_TRAIT)
        let key = ImplTraitsKey {
            name: typ.to_string(),
            path: String::from("out/src/main.rs"),
        };
        unsafe {
            if let Some(vc) = IMPL_TRAITS.get_mut(&key) {
                if vc.iter().any(|x| x.trt_name == expected_trait) {
                    return true
                }
            }

            let trt_pos = info.traits[expected_trait].pos;
            let trt_file = &info.traits[expected_trait].parent_file
                .strip_prefix("out/src/").unwrap()
                .rsplit_once('.').unwrap().0
                .replace('/', "::"); // todo \ for windows
            let trt_module = ast[trt_pos].ref_children()[1];
            let trt_funcs = get_trt_strct_functions(ast, &ast[trt_module]);

            let one_of_enum = info.one_of_enums.get(
                typ.to_string().as_str()
            ).unwrap();
            let generics = if one_of_enum.needs_lifetime {
                if one_of_enum.generics.is_empty() {
                    String::from("<'b_i_lifetime>")
                } else {
                    one_of_enum.generics.replacen('<', "<'b_i_lifetime, ", 1)
                }
            } else {
                one_of_enum.generics.clone()
            };

            let implementation = format!(
                "impl{generics} {trt_file}::{expected_trait} for {typ}{generics} {{ \n\t{} \n}}",
                join( // TODO generics !!!!!!
                    trt_funcs.iter().map(|(func_name, (_, func_typ))| {
                        let none = vec![];
                        let inputs = func_typ.input.as_ref().unwrap_or(&none);
                        let param = join(inputs.iter(), ", ");
                        let args = join(
                            inputs.iter()
                                .skip(1) //1 self // todo what if isnt self
                                .map(|x| &x.name),
                            ", "
                        );
                        let rtrn = if let Some(rtn) = &func_typ.output {
                            format!(" -> {rtn}")
                        } else { EMPTY_STR };
                        let typ_str = escape_typ_chars(&typ.to_string());
                        format!(
                            "fn {func_name}({param}) {rtrn} {{ \n\t\tmatch self {{\n\t\t\t{}\n\t\t}}\n\t}}",
                            join(typ.ref_children().iter().map(|t|
                                format!(
                                    "{typ_str}::_{}(x) => {trt_file}::{expected_trait}::{func_name}(x, {args}),",
                                    escape_typ_chars(&t.to_string())
                                )
                            ), "\n\t\t\t")
                        )
                    }),
                      "\n"
                ),

            );
            let val = ImplTraitsVal {
                trt_name: String::from(expected_trait),
                implementation: Some(implementation),
                types: None, // TODO !!!!!!!!!!!!!!!
            };
            if let Some(vc) = IMPL_TRAITS.get_mut(&key) {
                vc.push(val);
            } else {
                IMPL_TRAITS.insert(key, vec![val]);
            }
        }
        return true
    }

    match &typ.kind {
        TypeKind::Trait(name) => name == expected_trait,
        TypeKind::Struct(struct_name) => {
            let key = ImplTraitsKey {
                name: struct_name.to_string(),
                path: info.structs[struct_name.get_str()].parent_file.clone(),
            };
            unsafe { IMPL_TRAITS.get(&key) }.unwrap_or(&vec![]).iter().any(
                |x| x.trt_name == expected_trait
            ) || {
                let strct_def = info.structs[struct_name.get_str()].pos;
                let strct_module = ast[strct_def].ref_children()[2];
                let strct_funcs = get_trt_strct_functions(ast, &ast[strct_module]);
                let trt_pos = info.traits[expected_trait].pos;
                let trt_module = ast[trt_pos].ref_children()[1];
                let trt_funcs = get_trt_strct_functions(ast, &ast[trt_module]);
                // if let AstNode::Trait { strict: true, .. } = ast[trt_pos].value {
                //     // TODO if implements
                //     //  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                //     //  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                //     return false
                // }
                if let Some(types_hm) = struct_matches_trait(
                    &trt_funcs, &strct_funcs, expected_trait
                ) {
                    let implementation = add_trait_to_struct(
                        ast, struct_name.get_str(), &strct_funcs,
                        expected_trait, &trt_funcs, info
                    );
                    unsafe {
                        let val = ImplTraitsVal {
                            trt_name: String::from(expected_trait),
                            implementation: Some(implementation),
                            types: if types_hm.is_empty() { None } else { Some(types_hm) },
                        };
                        if let Some(vc) = IMPL_TRAITS.get_mut(&key) {
                            vc.push(val);
                        } else {
                            IMPL_TRAITS.insert(
                                key,
                                vec![val]
                            );
                        }
                    }
                    true
                } else { false }
            }
            // let struct_def = &ast[info.structs[struct_name.get_str()].pos];
            // let traits = &ast[unwrap_u(&struct_def.children)[3]];

            // unwrap_u(&traits.children).iter().any(|trt|
            //     matches!(&ast[*trt].value, AstNode::Identifier(name) if expected_trait == name)
            // )
        }
        TypeKind::Tuple => {
            ["Debug"].contains(&expected_trait)
        }
        _ => false
    }
}