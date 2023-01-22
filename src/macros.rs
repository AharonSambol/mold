#[macro_export] macro_rules! unwrap_enum {
    ($var:expr) => {
        if let Some(x) = $var { x } else { panic!() }
    };
    ($var:expr, $pattern:pat) => {
        let $pattern = $var else { panic!() };
    };
    ($var:expr, $pattern:pat, $result:expr) => {
        if let $pattern = $var { $result } else { panic!() }
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
        StructFunc::Struct(BuiltInStruct {
            name: stringify!($val),
            generics: None,
            methods: vec![
                concat!("clone() -> ", stringify!($val)),
                "__init__(self)"
            ],
            _parameters: vec![],
            types: None,
        })
    };
}
