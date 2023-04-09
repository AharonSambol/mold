use std::collections::{HashMap};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use pretty_print_tree::{Color, PrettyPrintTree};
use crate::construct_ast::ast_structure::{Ast, Param};
use crate::{add_trait, EMPTY_STR, IMPL_TRAITS, Implementation, ImplTraitsKey, ImplTraitsVal, typ_with_child, some_vec, unwrap_enum};
use crate::add_types::generics::apply_generics_from_base;
use crate::add_types::polymorphism::{escape_typ_chars, make_enums};
use crate::add_types::utils::{get_pointer_complete_inner, join};
use crate::construct_ast::mold_ast::{add_trait_to_struct, get_trt_strct_functions, Info, TraitFuncs};
use crate::{throw, CUR_COL, CUR_LINE, CUR_PATH, LINE_DIFF, SRC_CODE};

pub const UNKNOWN_TYPE: Type = Type {
    kind: TypeKind::Unknown,
    children: None
};
const EMPTY_PARAM: Param = Param {
    typ: UNKNOWN_TYPE, name: EMPTY_STR, is_mut: false, is_args: false, is_kwargs: false, default_val_pos: None
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
    AssociatedType(String), // e.g. Iterator[Inner=i32]
    VArgs,
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
            TypeKind::VArgs => {
                write!(f, "Vec<{}>", unwrap(&self.children)[0])
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
            TypeKind::AssociatedType(name) => {
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
                    for t in typ.children.unwrap() {
                        if !vc.contains(&t) {
                            vc.push(t.clone());
                        }
                    }
                    // vc.append(typ.children.as_mut().unwrap());
                } else {
                    throw!("adding two empty 'OneOf' types together??")
                }
            } else if let Some(vc) = &mut self.children {
                if !vc.contains(&typ) {
                    vc.push(typ);
                }
            } else { throw!("an empty 'OneOf' type?") }
            self
        } else if let TypeKind::OneOf = typ.kind {
            if let Some(vc) = &mut typ.children {
                if !vc.contains(&self) {
                    vc.insert(0, self)
                }
            } else { throw!("an empty 'OneOf' type?") }
            typ
        } else if self == typ {
            self
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

pub fn implements_trait(mut typ: &Type, expected_trait: &Type, ast: &[Ast], info: &Info) -> Option<Type> {
    #[inline] fn struct_matches_trait(trt_funcs: &TraitFuncs, funcs: &TraitFuncs, trait_name: &str) -> Option<(HashMap<String, Type>, HashMap<String, Type>)> {
        let mut a_types_hm = HashMap::new();
        let mut generics_hm = HashMap::new();
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

                if types_match(
                    &mut a_types_hm, &mut generics_hm,
                    &mut func_all_types.iter().map(|x| &x.typ),
                    &mut trt_f_all_types.iter().map(|x| &x.typ)
                ) {
                    continue 'trait_func_loop
                }
            }
            return None
        }
        Some((generics_hm, a_types_hm))
    }
    #[inline] fn types_match<'a>(
        mut a_types_hm: &'a mut HashMap<String, Type>,
        mut generics_hm: &'a mut HashMap<String, Type>,
        got_types: &mut dyn Iterator<Item=&Type>,
        exp_types: &mut dyn Iterator<Item=&Type>
    ) -> bool {
        for (exp, got) in exp_types.zip(got_types) {
            if exp == got {
                continue
            }
            let (name, hm) = match &exp.kind {
                TypeKind::AssociatedType(typ_name) => (typ_name, &mut a_types_hm),
                TypeKind::Generic(GenericType::NoVal(name)) => (name, &mut generics_hm),
                _ => return false
            };
            if let Some(expected_typ) = hm.get(name) {
                if got != expected_typ {
                    return false
                }
            } else {
                (*hm).insert(name.clone(), got.clone());
            }
        }
        true
    }

    let expected_trait_name = unwrap_enum!(&expected_trait.kind, TypeKind::Trait(trt), trt.get_str());
    if let TypeKind::Generic(GenericType::WithVal(_)) = &typ.kind {
        typ = &unwrap(&typ.children)[0];
    }

    if let TypeKind::Pointer | TypeKind::MutPointer = &typ.kind {
        if expected_trait_name == "Debug" { // TODO this probably has to do with dereferencing or smthing
            typ = get_pointer_complete_inner(typ);
        }
    }

    if typ == expected_trait {
        return Some(typ.clone())
    }

    if let TypeKind::OneOf = &typ.kind {
        return one_of_implements_trait(typ, expected_trait, ast, info, expected_trait_name)
    }

    match &typ.kind {
        TypeKind::Trait(name) => {
            if name != expected_trait_name {
                return None
            }

            let (got_generics, got_a_types) = get_generics_and_a_types(typ);
            let (expected_generics, expected_a_types) = get_generics_and_a_types_optional(expected_trait);

            for a_typ in got_a_types.keys() {
                let got_typ = &got_a_types[a_typ.as_str()];
                let expected_typ = &expected_a_types[a_typ.as_str()];

                if let Some(expected_typ) = expected_typ {
                    if got_typ == expected_typ {
                        continue
                    }
                } else {
                    continue
                }
            }
            for (got_gen, exp_gen) in got_generics.iter().zip(expected_generics) {
                let got_name = unwrap_enum!(&got_gen.kind, TypeKind::Generic(GenericType::WithVal(n)), n);
                let exp_name = unwrap_enum!(&exp_gen.kind, TypeKind::Generic(GenericType::WithVal(n)), n);
                if got_name != exp_name {
                    return None
                }
                if let Some(children) = exp_gen.children {
                    if got_gen.ref_children()[0] == children[0] {
                        continue
                    }
                    return None
                } else {
                    continue
                }
            }
            return Some(typ.clone())
        },
        TypeKind::Struct(struct_name) => {
            let (expected_generics, expected_a_types) = get_generics_and_a_types_optional(expected_trait);

            let key = ImplTraitsKey {
                name: struct_name.to_string(),
                path: info.structs[struct_name.get_str()].parent_file.clone(),
            };
            unsafe {
                if let Some(vc) = IMPL_TRAITS.get(&key) {
                    let expected_all_types = join_generics_and_types(
                        &Some(expected_generics),
                        &mut expected_a_types.iter().map(|(name, typ)|
                            (name, typ.as_ref().unwrap())
                        )
                    );
                    for trt in vc.iter().filter(|x| x.trt_name == expected_trait_name) {
                        let mut a_types_hm = HashMap::new();
                        let mut generics_hm = HashMap::new();
                        let trt_all_types = join_generics_and_types(
                            &trt.generics, &mut trt.types.as_ref().unwrap_or(&HashMap::new()).iter(),
                        );

                        if types_match(
                            &mut a_types_hm, &mut generics_hm,
                            &mut expected_all_types.iter(),
                            &mut trt_all_types.iter()
                        ) {
                            let res = Some(typ_with_child! {
                                TypeKind::Trait(TypName::Str(String::from(expected_trait_name))),
                                Type {
                                    kind: TypeKind::GenericsMap,
                                    children: if trt_all_types.is_empty() { None } else { Some(trt_all_types) }
                                }
                            });
                            if let Some(res) = apply_generics_from_base(&res, typ) {
                                return Some(res)
                            }
                            return res
                        }
                    }
                }
            }

            let trait_generics = &info.traits[expected_trait_name].generics;
            if matches!(trait_generics, Some(vc) if !vc.is_empty()) {
                return None //1 doesnt (yet) support duck typing for traits with generics
            }

            let strct_def = info.structs[struct_name.get_str()].pos;
            let strct_module = ast[strct_def].ref_children()[2];
            let strct_funcs = get_trt_strct_functions(ast, &ast[strct_module]);
            let trt_pos = info.traits[expected_trait_name].pos;
            let trt_module = ast[trt_pos].ref_children()[1];
            let trt_funcs = get_trt_strct_functions(ast, &ast[trt_module]);

            let types_hm = struct_matches_trait(
                &trt_funcs, &strct_funcs, expected_trait_name
            );
            types_hm.as_ref()?;
            let (generics_hm, a_types_hm) = types_hm.unwrap();
            let generics_vec = if generics_hm.is_empty() { None } else { Some(generics_hm.values().cloned().collect()) };
            let a_types_hm = if a_types_hm.is_empty() { None } else { Some(a_types_hm) };
            let implementation = add_trait_to_struct(
                ast, struct_name.get_str(), &strct_funcs,
                expected_trait_name, &trt_funcs, &a_types_hm, &generics_vec,
                info
            );
            let val = ImplTraitsVal {
                trt_name: String::from(expected_trait_name),
                implementation: Implementation::Is(implementation),
                types: a_types_hm.clone(),
                generics: generics_vec.clone()
            };
            add_trait!(key, val);
            let mut children = unwrap(&generics_vec).clone();
            children.extend(a_types_hm.unwrap_or_default().iter().map(
                |(name, val)| Type {
                    kind: TypeKind::AssociatedType(name.clone()),
                    children: Some(vec![val.clone()])
                }
            ));

            Some(typ_with_child! {
                TypeKind::Trait(TypName::Str(String::from(expected_trait_name))),
                Type {
                    kind: TypeKind::GenericsMap,
                    children: if children.is_empty() { None } else { Some(children) }
                }
            })
        }
        TypeKind::Tuple => {
            if ["Debug"].contains(&expected_trait_name) {
                Some(expected_trait.clone())
            } else { None }
        }
        _ => None
    }
}

fn one_of_implements_trait(typ: &Type, expected_trait: &Type, ast: &[Ast], info: &Info, expected_trait_name: &str) -> Option<Type> {
    let mut options = typ.ref_children().iter();
    let first = implements_trait(options.next().unwrap(), expected_trait, ast, info);
    if first.is_none() || options.any(|typ| implements_trait(typ, expected_trait, ast, info) != first) {
        return None
    }
    let first = first.unwrap();

    let (trt_generics, trt_a_types) = get_generics_and_a_types(&first);

    let trt_generics = if trt_generics.is_empty() { None } else { Some(trt_generics) };
    let trt_a_types = if trt_a_types.is_empty() { None } else { Some(trt_a_types) };
    // TODO when implementing Display for a one_of_enum needs an actual fn impl
    // TODO ignore some implementations (IGNORE_STRUCT/ IGNORE_TRAIT)
    let key = ImplTraitsKey {
        name: typ.to_string(),
        path: String::from("out/src/main.rs"),
    };
    if let Some(vc) = unsafe { IMPL_TRAITS.get_mut(&key) } {
        if let Some(trt) = vc.iter().find(|x| x.trt_name == expected_trait_name && x.generics == trt_generics) {
            if trt.types != trt_a_types {
                throw!("cant impl same trait twice with different associated types (I dont think this should ever happen..)")
            }
            return Some(first)
            // let mut children = unwrap(&trt.generics).clone();
            // children.extend(
            //     trt.types.unwrap_or_default().iter().map(
            //         |(name, typ)| typ_with_child! {
            //             TypeKind::InnerType(name.clone()),
            //             typ.clone()
            //         }
            //     )
            // );
            // return Some(typ_with_child! {
            //     TypeKind::Trait(TypName::Str(String::from(expected_trait))),
            //     Type {
            //         kind: TypeKind::GenericsMap,
            //         children: if children.is_empty() { None } else { Some(children) },
            //     }
            // })
        }
    }

    let trt_pos = info.traits[expected_trait_name].pos;
    let trt_file = &info.traits[expected_trait_name].parent_file
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
        join(
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
        trt_name: String::from(expected_trait_name),
        implementation: Implementation::Is(implementation),
        types: trt_a_types,
        generics: trt_generics,
    };
    add_trait!(key, val);
    Some(first)
}

fn get_generics_and_a_types(typ: &Type) -> (Vec<Type>, HashMap<String, Type>){
    let mut trt_generics = vec![];
    let mut trt_a_types = HashMap::new();

    let generics_map = &typ.children.as_ref().unwrap()[0];
    for child in generics_map.children.clone().unwrap_or_default() {
        match &child.kind {
            TypeKind::Generic(GenericType::WithVal(_)) => trt_generics.push(child),
            TypeKind::AssociatedType(name) => {
                trt_a_types.insert(name.clone(), child.ref_children()[0].clone());
            },
            _ => unreachable!()
        }
    }
    (trt_generics, trt_a_types)
}


fn get_generics_and_a_types_optional(expected_trait: &Type) -> (Vec<Type>, HashMap<String, Option<Type>>) {
    let mut expected_generics = vec![];
    let mut expected_a_types = HashMap::new();

    let generics_map = &expected_trait.children.as_ref().unwrap()[0];
    for child in unwrap(&generics_map.children) {
        match &child.kind {
            TypeKind::Generic(GenericType::WithVal(_)) => expected_generics.push(child.clone()),
            TypeKind::AssociatedType(name) =>
                if let Some(children) = &child.children {
                    expected_a_types.insert(name.clone(), Some(children[0].clone()));
                } else {
                    expected_a_types.insert(name.clone(), None);
                },
            _ => unreachable!()
        }
    }
    (expected_generics, expected_a_types)
}
fn join_generics_and_types(generics: &Option<Vec<Type>>, a_types: &mut dyn Iterator<Item=(&String, &Type)>) -> Vec<Type> {
    let mut res = generics.clone().unwrap_or_default();
    res.extend(a_types.map(|(name, typ)|
        typ_with_child! {
            TypeKind::AssociatedType(name.clone()),
            typ.clone()
        }
    ));

    res
}

pub fn join_types<T: Iterator<Item=Type>>(mut types: T, info: &mut Info) -> Type {
    let res = if let Some(t) = types.next() { t } else { return UNKNOWN_TYPE };
    let mut res = if let TypeKind::OneOf = res.kind {
        res
    } else {
        typ_with_child!{
            TypeKind::OneOf,
            res
        }
    };
    for t in types {
        res = res.add_option(t);
    }
    // panic!("{:?}", res);
    if res.ref_children().len() == 1 {
        res.children.unwrap().remove(0)
    } else {
        make_enums(&res, info.one_of_enums);
        res
    }
}