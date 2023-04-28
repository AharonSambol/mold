#[macro_export] macro_rules! unwrap_enum {
    ($var:expr, $pattern:pat) => {
        assert!(matches!($var, $pattern));
    };
    ($var:expr, $pattern:pat, $result:expr) => {
        if let $pattern = $var { $result } else { unreachable!() }
    };
    ($var:expr, $pattern:pat, $result:expr, $msg:expr) => {
        if let $pattern = $var { $result } else { throw!($msg) }
    };
    ($var:expr, $pattern:pat, $result:expr, $msg:expr, $($arg:tt)*) => {
        if let $pattern = $var { $result } else { throw!($msg, $($arg)*) }
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
                "fmt(self: &Self, f: &mut Formatter[`_]) -> Result[(), Error]"
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

#[macro_export]
macro_rules! throw { // todo repeating code
    () => {
        { #[allow(unused_unsafe)] unsafe {
            if *LINE_DIFF.last().unwrap() > CUR_LINE {
                let ln1 = CUR_LINE.to_string().len();
                let ln2 = CUR_COL.to_string().len();
                let padding = ln1.max(ln2);
                panic!(
                    concat!(
                        "{}:{}:{}\x1b[94m\x1b[1m ***INTERNAL ERROR***:\x1b[0m \n\
                        \x1b[94m\x1b[1mline {:width$} |\x1b[0m \x1b[1m\x1b[31m{} \x1b[0m \n\
                        \x1b[94m\x1b[1mcol  {:width$} | {}^ (about here)\x1b[0m \n"
                    ),
                    CUR_PATH.last().unwrap().strip_prefix("out/src/").unwrap(),
                    CUR_LINE + 1, CUR_COL + 1,
                    CUR_LINE + 1,
                    SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap(),
                    CUR_COL,
                    " ".repeat(CUR_COL),
                    width = padding,
                )
            }
            let line = CUR_LINE - LINE_DIFF.last().unwrap();
            let ln1 = line.to_string().len();
            let ln2 = CUR_COL.to_string().len();
            let padding = ln1.max(ln2);

            panic!(
                concat!(
                    "{}:{}:{}\x1b[94m\x1b[1m error:\x1b[0m \n\
                    \x1b[94m\x1b[1mline {:width$} |\x1b[0m \x1b[1m\x1b[31m{} \x1b[0m \n\
                    \x1b[94m\x1b[1mcol  {:width$} | {}^ (about here)\x1b[0m \n"
                ),
                // MODULE_PATH.as_ref().unwrap().to_str().unwrap(),
                CUR_PATH.last().unwrap().strip_prefix("out/src/").unwrap(),
                line + 1, CUR_COL + 1,
                line + 1,
                SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap(),
                CUR_COL,
                " ".repeat(CUR_COL),
                width = padding,
            )
        }}
    };
    ($st:tt) => {
        { #[allow(unused_unsafe)] unsafe {
            if *LINE_DIFF.last().unwrap() > CUR_LINE {
                let ln1 = CUR_LINE.to_string().len();
                let ln2 = CUR_COL.to_string().len();
                let padding = ln1.max(ln2);
                panic!(
                    concat!(
                        "\x1b[1m\x1b[91m", $st, "\x1b[0m\n\
                        {}:{}:{}\x1b[94m\x1b[1m ***INTERNAL ERROR***:\x1b[0m \n\
                        \x1b[94m\x1b[1mline {:width$} |\x1b[0m \x1b[1m\x1b[31m{} \x1b[0m \n\
                        \x1b[94m\x1b[1mcol  {:width$} | {}^ (about here)\x1b[0m \n"
                    ),
                    CUR_PATH.last().unwrap().strip_prefix("out/src/").unwrap(),
                    CUR_LINE + 1, CUR_COL + 1,
                    CUR_LINE + 1,
                    SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap(),
                    CUR_COL,
                    " ".repeat(CUR_COL),
                    width = padding,
                )
            }
            let line = CUR_LINE - LINE_DIFF.last().unwrap();
            let ln1 = line.to_string().len();
            let ln2 = CUR_COL.to_string().len();
            let padding = ln1.max(ln2);

            panic!(
                concat!(
                    "\x1b[1m\x1b[91m", $st, "\x1b[0m\n\
                    {}:{}:{}\x1b[94m\x1b[1m error:\x1b[0m \n\
                    \x1b[94m\x1b[1mline {:width$} |\x1b[0m \x1b[1m\x1b[31m{} \x1b[0m \n\
                    \x1b[94m\x1b[1mcol  {:width$} | {}^ (about here)\x1b[0m \n"
                ),
                // MODULE_PATH.as_ref().unwrap().to_str().unwrap(),
                CUR_PATH.last().unwrap().strip_prefix("out/src/").unwrap(),
                line + 1, CUR_COL + 1,
                line + 1,
                SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap(),
                CUR_COL,
                " ".repeat(CUR_COL),
                width = padding,
            )
        }}
    };
    ($format:tt, $($arg:tt)*) => {
        { #[allow(unused_unsafe)] unsafe {
            if *LINE_DIFF.last().unwrap() > CUR_LINE {
                let ln1 = CUR_LINE.to_string().len();
                let ln2 = CUR_COL.to_string().len();
                let padding = ln1.max(ln2);
                panic!(
                    concat!(
                        "\x1b[1m\x1b[91m", $format, "\x1b[0m\n\
                        {}:{}:{}\x1b[94m\x1b[1m ***INTERNAL ERROR***:\x1b[0m \n\
                        \x1b[94m\x1b[1mline {:width$} |\x1b[0m \x1b[1m\x1b[31m{} \x1b[0m \n\
                        \x1b[94m\x1b[1mcol  {:width$} | {}^ (about here)\x1b[0m \n"
                    ),
                    $($arg)*,
                    CUR_PATH.last().unwrap().strip_prefix("out/src/").unwrap(),
                    CUR_LINE + 1, CUR_COL + 1,
                    CUR_LINE + 1,
                    SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap(),
                    CUR_COL,
                    " ".repeat(CUR_COL),
                    width = padding,
                )
            }
            let line = CUR_LINE - LINE_DIFF.last().unwrap();
            let ln1 = line.to_string().len();
            let ln2 = CUR_COL.to_string().len();
            let padding = ln1.max(ln2);

            panic!(
                concat!(
                    "\x1b[1m\x1b[91m", $format, "\x1b[0m\n\
                    {}:{}:{}\x1b[94m\x1b[1m error:\x1b[0m \n\
                    \x1b[94m\x1b[1mline {:width$} |\x1b[0m \x1b[1m\x1b[31m{} \x1b[0m \n\
                    \x1b[94m\x1b[1mcol  {:width$} | {}^ (about here)\x1b[0m \n"
                ),
                $($arg)*,
                CUR_PATH.last().unwrap().strip_prefix("out/src/").unwrap(),
                line + 1, CUR_COL + 1,
                line + 1,
                SRC_CODE.last().unwrap().split('\n').nth(CUR_LINE).unwrap(),
                CUR_COL,
                " ".repeat(CUR_COL),
                width = padding,
            );
        }}
    };
}

#[macro_export] macro_rules! add_trait {
    ($key: expr, $val: expr) => {
        unsafe {
            if let Some(vc) = IMPL_TRAITS.get_mut(&$key) {
                if let Some(trt) = vc.iter().find(|x| x.trt_name == $val.trt_name && x.generics == $val.generics) {
                    if trt.types != $val.types {
                        throw!("cant implement same trait twice with different associated types")
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
