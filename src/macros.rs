#[macro_export] macro_rules! unwrap_enum {
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