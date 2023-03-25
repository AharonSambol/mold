#[macro_export] macro_rules! unwrap_enum {
    ($var:expr, $pattern:pat) => {
        let $pattern = $var else { unreachable!() };
    };
    ($var:expr, $pattern:pat, $result:expr) => {
        if let $pattern = $var { $result } else { unreachable!() }
    };
    ($var:expr, $pattern:pat, $result:expr, $msg:expr) => {
        if let $pattern = $var { $result } else { panic!($msg) }
    };
}

#[macro_export] macro_rules! some_vec {
    () => {
        Some(vec![])
    };
    ($($x:expr),+ $(,)?) => {
        Some(vec![$($x),+])
    }
}

#[macro_export] macro_rules! typ_with_child {
    ($kind:expr, $children:expr) => {
        Type {
            kind: $kind,
            children: some_vec![ $children ]
        }
    }
}

#[macro_export] macro_rules! make_primitive {
    ($val:expr) => {
        BuiltIn::Struct(BuiltInStruct {
            name: stringify!($val),
            generics: None,
            methods: vec![
                concat!("clone() -> ", stringify!($val)),
                "__init__(self)",
                // "__str__(self: &Self) -> str",
                "fmt(self: &Self, f: &mut Formatter['_]) -> Result[(), Error]"
            ],
            methods_with_impl: vec![],
            traits: Some(vec![
                "Debug", "Display"
            ]),
        })
    };
}

#[macro_export] macro_rules! get_traits {
    ($struct_name: expr, $info: expr) => {
        unsafe {
            IMPL_TRAITS.get(
                &ImplTraitsKey {
                    name: $struct_name.to_string(),
                    path: $info.structs[$struct_name.get_str()].parent_file.clone(),
                }
            )
        }
    }
}

#[macro_export] macro_rules! add_trait {
    ($key: expr, $val: expr) => {
        unsafe {
            if let Some(vc) = IMPL_TRAITS.get_mut(&$key) {
                if let Some(trt) = vc.iter().find(|x| x.trt_name == $val.trt_name && x.generics == $val.generics) {
                    if trt.types != $val.types {
                        panic!("cant implement same trait twice with different associated types")
                    }
                } else {
                    vc.push($val);
                }
            } else {
                IMPL_TRAITS.insert($key, vec![$val]);
            }
        }
    }
}
