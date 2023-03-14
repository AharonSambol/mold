use std::fmt::{Display, Formatter};
use std::fmt::Write;
use std::hint::unreachable_unchecked;
use crate::{EMPTY_STR, IS_COMPILED};
use crate::mold_tokens::Token::{Brace, Bracket, Colon, Comma, Num, NewLine, Operator, Parenthesis, Period, Tab, Word, Str, Char, LifeTime};


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IsOpen { True, False }


#[derive(Debug)]
enum Token {
    Brace(IsOpen), Bracket(IsOpen), Parenthesis(IsOpen),
    Str {
        start: usize,
        end: usize,
        mutable: bool
    },
    Char(char),
    LifeTime,
    Num {
        start: usize,
        end: usize,
        is_spaced: bool
    },
    Word {
        start: usize,
        end: usize,
        is_spaced: bool
    },
    Operator {
        start: usize,
        end: usize,
        is_spaced: bool
    },
    Colon, Comma, Period,
    Tab, NewLine
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolidToken {
    Brace(IsOpen), Bracket(IsOpen), Parenthesis(IsOpen),
    Word(String), LifeTime(String),
    Str{
        val: String,
        mutable: bool
    },
    Char(String),
    Num(String), // Float(f32),
    Bool(bool),
    Operator(OperatorType), UnaryOperator(OperatorType),
    Colon, Comma, Period,
    Tab, NewLine,
    Def, Class, Enum, Struct, Trait, StrictTrait, Type,
    If, Else, Elif,
    Match, Case, While, For,
    Break, Continue, Return, Pass,
    Cast, In, IMut,
    From, Import, As,
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OperatorType {
    Eq, IsEq, Bigger, Smaller, NEq, BEq, SEq,
    Plus, Minus, Mul, Pow, Div, Mod, FloorDiv,
    OpEq(Box<OperatorType>),
    // PlusEq, MinusEq, MulEq, PowEq, DivEq, ModEq, FloorDivEq,
    BinOr, BinAnd, Xor, BinNot,
    OrEq, AndEq, XorEq,
    ShiftR, ShiftL,
    And, Or, Not, Is, In, IsNot, NotIn,
    Returns,
    MutPointer, Pointer, Dereference
}

// todo is, in, not
impl Display for OperatorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let OperatorType::OpEq(op) = self {
            return write!(f, "{}=", **op)
        }
        write!(f, "{}", match self {
            OperatorType::Eq => "=",
            OperatorType::IsEq => "==",
            OperatorType::Bigger => ">",
            OperatorType::Smaller => "<",
            OperatorType::NEq => "!=",
            OperatorType::BEq => ">=",
            OperatorType::SEq => "<=",
            OperatorType::Plus => "+",
            OperatorType::Minus => "-",
            OperatorType::Mul => "*",
            OperatorType::Pow => if unsafe { IS_COMPILED } { todo!() } else { "**" },
            OperatorType::Div => "/",
            OperatorType::Mod => "%",
            OperatorType::FloorDiv => if unsafe { IS_COMPILED } { "/" } else { "//" },
            OperatorType::BinOr => "|",
            OperatorType::Pointer => "&", //1 python ignores this
            OperatorType::MutPointer => "&mut ", //1 python ignores this
            OperatorType::Dereference => "*", //1 python ignores this
            OperatorType::BinAnd => "&",
            OperatorType::Xor => "^",
            OperatorType::BinNot => if unsafe { IS_COMPILED } { "!" } else { "~" },
            OperatorType::OrEq => "|=",
            OperatorType::AndEq => "&=",
            OperatorType::XorEq => "^=",
            OperatorType::ShiftL => "<<",
            OperatorType::ShiftR => ">>",
            OperatorType::Returns => "->",
            OperatorType::And => if unsafe { IS_COMPILED } { "&&" } else { " and " },
            OperatorType::Or => if unsafe { IS_COMPILED } { "||" } else { " or " },
            OperatorType::Not => if unsafe { IS_COMPILED } { "!" } else { " not " },
            OperatorType::Is => " is ",
            OperatorType::In => " in ",
            OperatorType::IsNot => " is not ",
            OperatorType::NotIn => " not in ",
            OperatorType::OpEq(_) => unreachable!(),
        })
    }
}
impl OperatorType {
    pub(crate) fn get_priority(&self) -> i16 {
        match self {
            OperatorType::Pow => 100,
            OperatorType::BinNot => 95,
            OperatorType::Mul
            | OperatorType::Div
            | OperatorType::Mod
            | OperatorType::FloorDiv => 90,
            OperatorType::Plus
            | OperatorType::Minus => 80,
            OperatorType::ShiftL
            | OperatorType::ShiftR => 70,
            OperatorType::BinAnd => 60,
            OperatorType::Xor => 50,
            OperatorType::BinOr => 40,
            OperatorType::IsEq
            | OperatorType::Bigger
            | OperatorType::Smaller
            | OperatorType::NEq
            | OperatorType::BEq
            | OperatorType::SEq
            | OperatorType::In
            | OperatorType::Is
            | OperatorType::IsNot
            | OperatorType::NotIn => 30,
            OperatorType::Not => 25,
            OperatorType::And => 20,
            OperatorType::Or => 10,



            _ => -100
        }
    }
}

enum Comment { None, Normal, Multiline(i8) }

pub fn tokenize(input_code: &str) -> Vec<SolidToken> {
    let mut tokens = Vec::new();
    let mut skip = 0;
    let mut is_str = false;
    let mut is_comment = Comment::None;
    let mut escaped = false;
    let mut open_braces = 0;
    let mut open_brackets = 0;
    let mut open_parentheses = 0;
    let chars: Vec<_> = input_code.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        if skip > 0 {
            skip -= 1;
            continue
        }
        if let Comment::Normal = is_comment {
            if *c == '\n' {
                is_comment = Comment::None
            } else { continue }
        }
        if let Comment::Multiline(amount) = is_comment {
            if *c == '#' {
                if chars[i - 1] == '}' {
                    if amount == 1 {
                        is_comment = Comment::None;
                    } else {
                        is_comment = Comment::Multiline(amount - 1);
                    }
                } else if i + 1 < chars.len() && chars[i + 1] == '{' {
                    is_comment = Comment::Multiline(amount + 1);
                }
            }
            continue
        }

        if is_str {
            match c {
                '"' => {
                    is_str = escaped;
                    escaped = false;
                },
                '\\' => escaped = !escaped,
                _ => escaped = false
            }
            if let Str { end, .. } = tokens.last_mut().unwrap() { // always true
                *end += 1;
            }
            continue
        }

        let token = match c {
            '#' => {
                if i + 1 < chars.len() && chars[i + 1] == '{' {
                    skip = 1;
                    is_comment = Comment::Multiline(1);
                } else {
                    is_comment = Comment::Normal;
                }
                continue
            },
            ' ' => {
                space_prev_token(&mut tokens);
                if is_tab(&chars, i) {
                    skip = 3;
                    Tab
                } else { continue }
            },
            '\t' => Tab,   '\n' => NewLine,
            ':' => Colon,   ',' => Comma,   '.' => Period,
            '0'..='9' => Num { start: i, end: i + 1, is_spaced: false },
            // <editor-fold desc="+-*%">
            '+' | '-' | '*' | '&' | '^' | '%' | '!' | '|' | '/' | '=' | '>' | '<' | '~' => Operator {
                start: i, end: i + 1, is_spaced: false
            },
            // </editor-fold>
            // <editor-fold desc="[]{}()">
            '[' => {
                open_brackets += 1;
                Bracket(IsOpen::True)
            },
            ']' => {
                open_brackets -= 1;
                if open_brackets == -1 {    panic!("unexpected close bracket") }
                Bracket(IsOpen::False)
            },
            '{' => {
                open_braces += 1;
                Brace(IsOpen::True)
            },
            '}' => {
                open_braces -= 1;
                if open_braces == -1 {    panic!("unexpected close braces") }
                Brace(IsOpen::False)
            },
            '(' => {
                open_parentheses += 1;
                Parenthesis(IsOpen::True)
            },
            ')' => {
                open_parentheses -= 1;
                if open_parentheses == -1 {    panic!("unexpected close parentheses") }
                Parenthesis(IsOpen::False)
            },
            // </editor-fold>
            // <editor-fold desc="''">
            '\'' => {
                if chars[i + 2] == '\'' {
                    make_char(&mut tokens, &mut skip, &chars, i);
                    continue
                } else {
                    LifeTime
                }
            },
            '"' => {
                is_str = true;
                if let Some(Word { start, end, is_spaced: false }) = tokens.last_mut() {
                    if *start == *end - 1 && chars[*start] == 'i' {
                        tokens.pop();
                        tokens.push(Str {
                            start: i, end: i + 1, mutable: false
                        });
                        continue
                    }
                }
                tokens.push(Str {
                    start: i,
                    end: i + 1,
                    mutable: true
                });
                continue
            },
            // </editor-fold>
            _ => Word { start: i, end: i + 1, is_spaced: false }
        };
        match token {
            Operator { is_spaced: false, .. } if join_op(&mut tokens, i + 1) => (),
            Word { is_spaced: false, .. } if join_to_word(&mut tokens, i + 1) => (),
            Word { is_spaced: false, .. } if join_to_num(&mut tokens, i + 1) => (),
            Num {..} if join_num(&mut tokens, i + 1) => (),
            Num {..} if join_num_to_word(&mut tokens, i + 1) => (),
            Period if join_num(&mut tokens, i + 1) => (),
            _ => tokens.push(token)
        };
    }
    solidify_tokens(&tokens, input_code)
}

fn solidify_tokens(tokens: &Vec<Token>, input_code: &str) -> Vec<SolidToken> {
    let mut res = Vec::with_capacity(tokens.len());
    let mut is_empty_line = true;
    let mut open = 0;
    for (i, token) in tokens.iter().enumerate() {
        if !matches!(token, Tab | NewLine) {
            is_empty_line = false;
        }
        if is_mut_pointer(tokens, &res, i, input_code) {
            *res.last_mut().unwrap() = SolidToken::UnaryOperator(OperatorType::MutPointer);
            continue
        }
        let st = match token {
            LifeTime => {
                let Word { .. } = tokens[i + 1] else {
                    panic!("expected identifier after `'`")
                };
                SolidToken::LifeTime(EMPTY_STR)
            }
            Parenthesis(is_open) | Bracket(is_open) | Brace(is_open) => {
                open += if let IsOpen::True = is_open { 1 } else { -1 };
                match token {
                    Parenthesis(_) => SolidToken::Parenthesis(is_open.clone()),
                    Bracket(_) => SolidToken::Bracket(is_open.clone()),
                    Brace(_) => SolidToken::Brace(is_open.clone()),
                    _ => unsafe { unreachable_unchecked() }
                }
            }
            Char(chr) => SolidToken::Char(clean_char(*chr)),
            Word { start, end, .. } => {
                let st = slice(input_code, *start, *end);
                match st.as_str() {
                    "def" => SolidToken::Def, "class" => SolidToken::Class,
                    "enum" => SolidToken::Enum, "struct" => SolidToken::Struct,
                    "trait" => SolidToken::Trait, "TRAIT" => SolidToken::StrictTrait,
                    "type" => SolidToken::Type,
                    "if" => SolidToken::If, "else" => SolidToken::Else,
                    "elif" => SolidToken::Elif,
                    "match" => SolidToken::Match, "case" => SolidToken::Case,
                    "while" => SolidToken::While, "for" => SolidToken::For,
                    "break" => SolidToken::Break, "continue" => SolidToken::Continue,
                    "return" => SolidToken::Return, "pass" => SolidToken::Pass,
                    "and" => SolidToken::Operator(OperatorType::And),
                    "or" => SolidToken::Operator(OperatorType::Or),
                    "not" => {
                        if let Some(SolidToken::Operator(OperatorType::Is)) = res.last() {
                            res.pop();
                            SolidToken::Operator(OperatorType::IsNot)
                        } else {
                            SolidToken::UnaryOperator(OperatorType::Not)
                        }
                    },
                    "is" => SolidToken::Operator(OperatorType::Is),
                    "in" => {
                        if let Some(SolidToken::UnaryOperator(OperatorType::Not)) = res.last() {
                            res.pop();
                            SolidToken::Operator(OperatorType::NotIn)
                        } else {
                            SolidToken::In
                        }
                    },
                    "True" | "true" => SolidToken::Bool(true),
                    "False" | "false" => SolidToken::Bool(false),
                    "imut" => SolidToken::IMut, "cast" => SolidToken::Cast,
                    "from" => SolidToken::From, "import" => SolidToken::Import,
                    "as" => SolidToken::As, "None" => SolidToken::Null,
                    _ => {
                        if let Some(SolidToken::LifeTime(lf)) = res.last_mut() {
                            *lf = format!("'{st}");
                            continue
                        }
                        // TODO if reserved_words.contains(st) { panic }
                        SolidToken::Word(clean(&st))
                    }
                }
            },
            Str { start, end, mutable: m } => SolidToken::Str {
                val: slice(input_code, *start, *end),
                mutable: *m
            },
            Operator { start, end, .. } => {
                // todo this is messy...
                let mut oper = String::new();
                for c in input_code.chars().skip(*start).take(*end-*start) {
                    if str_to_op_type(&format!("{oper}{c}")).is_none() && c != '!' {
                        let op = str_to_op_type(&oper).unwrap_or_else(||
                            panic!("invalid operator {oper}")
                        );
                        res.push(unary_or_bin(&res, op));
                        oper = c.to_string()
                    } else {
                        write!(&mut oper, "{c}").unwrap();
                    }
                }
                if let Some(op) = str_to_op_type(&oper) {
                    unary_or_bin(&res, op)
                } else {
                    panic!("invalid operator {oper}")
                }
            },
            Num { start, end, .. } => SolidToken::Num(slice(input_code, *start, *end)),
            Colon => SolidToken::Colon, Comma => SolidToken::Comma, Period => SolidToken::Period,
            Tab => if open == 0 { SolidToken::Tab } else { continue },
            NewLine => {
                if open != 0 { continue }
                while let Some(SolidToken::Tab) = res.last() {
                    res.pop();
                }
                if is_empty_line {
                    continue
                }
                is_empty_line = true;
                SolidToken::NewLine
            }
        };
        res.push(st);
    }
    if !matches!(res.last(), Some(SolidToken::NewLine)) {
        res.push(SolidToken::NewLine)
    }
    res
}

fn unary_or_bin(res: &Vec<SolidToken>, op: OperatorType) -> SolidToken {
    let mut idx = res.len() - 1;
    while let SolidToken::Tab | SolidToken::NewLine = res[idx] {
        if idx == 0 {
            panic!("Unexpected operator at start of file")
        }
        idx -= 1;
    }
    if let OperatorType::Eq = op {
        if let SolidToken::Colon = res[idx] {
            return SolidToken::Operator(op)
        }
    }
    if let SolidToken::Operator(_)
    | SolidToken::UnaryOperator(_)
    | SolidToken::Bracket(IsOpen::True)
    | SolidToken::Brace(IsOpen::True)
    | SolidToken::Parenthesis(IsOpen::True)
    | SolidToken::Comma
    | SolidToken::Return
    | SolidToken::While
    | SolidToken::If
    | SolidToken::Elif
    | SolidToken::For
    | SolidToken::In
    | SolidToken::Colon = res[idx] {
        match op {
            OperatorType::Minus | OperatorType::BinNot =>
                SolidToken::UnaryOperator(op),
            OperatorType::BinAnd =>
                SolidToken::UnaryOperator(OperatorType::Pointer),
            OperatorType::Mul =>
                SolidToken::UnaryOperator(OperatorType::Dereference),
            _ => panic!("Invalid unary operator {op}")
        }
    } else {
        SolidToken::Operator(op)
    }
}

fn str_to_op_type(st: &str) -> Option<OperatorType> {
    Some(match st {
        "=" => OperatorType::Eq,        "==" => OperatorType::IsEq,
        ">" => OperatorType::Bigger,    "<" => OperatorType::Smaller,
        "!=" => OperatorType::NEq,  ">=" => OperatorType::BEq,      "<=" => OperatorType::SEq,
        "+" => OperatorType::Plus,  "-" => OperatorType::Minus,     "*" => OperatorType::Mul,
        "/" => OperatorType::Div,   "//" => OperatorType::FloorDiv,
        "**" => OperatorType::Pow,  "%" => OperatorType::Mod,
        "+=" => OperatorType::OpEq(Box::new(OperatorType::Plus)),
        "-=" => OperatorType::OpEq(Box::new(OperatorType::Minus)),
        "*=" => OperatorType::OpEq(Box::new(OperatorType::Mul)),
        "/=" => OperatorType::OpEq(Box::new(OperatorType::Div)),
        "//=" => OperatorType::OpEq(Box::new(OperatorType::FloorDiv)),
        "**=" => OperatorType::OpEq(Box::new(OperatorType::Pow)),
        "%=" => OperatorType::OpEq(Box::new(OperatorType::Mod)),
        "|" => OperatorType::BinOr,    "&" => OperatorType::BinAnd,
        "^" => OperatorType::Xor,   "~" => OperatorType::BinNot,
        "|=" => OperatorType::OrEq, "&=" => OperatorType::AndEq,    "^=" => OperatorType::XorEq,
        ">>" => OperatorType::ShiftR, "<<" => OperatorType::ShiftL,
        "->" => OperatorType::Returns,
        _ => return None
    })
}

fn make_char(tokens: &mut Vec<Token>, skip: &mut i32, chars: &[char], i: usize) {
    if chars[i + 1] != '\\' {
        *skip = 2;
        tokens.push(Char(chars[i + 1]));
        return;
    }
    *skip = 3;
    tokens.push(Char(
        match chars[i + 2] {
            '\'' => '\'',
            't' => '\t',
            '\\' => '\\',
            'n' => '\n',
            'r' => '\r',
        _ => panic!("Invalid escape sequence \\{}", chars[i + 2])
        }
    ));
}

fn join_num(tokens: &mut [Token], new_end: usize) -> bool {
    if let Some(Num { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn join_num_to_word(tokens: &mut [Token], new_end: usize) -> bool {
    if let Some(Word { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn is_tab(chars: &Vec<char>, i: usize) -> bool {
    chars.len() > i + 3 && chars[i + 1] == ' ' && chars[i + 2] == ' ' && chars[i + 3] == ' '
}

fn space_prev_token(tokens: &mut [Token]) {
    if let Some(Operator { is_spaced, .. }) = tokens.last_mut() {
        *is_spaced = true;
    } else if let Some(Word { is_spaced, .. }) = tokens.last_mut() {
        *is_spaced = true;
    } else if let Some(Num { is_spaced, .. }) = tokens.last_mut() {
        *is_spaced = true;
    }
}

fn join_to_word(tokens: &mut [Token], new_end: usize) -> bool {
    if let Some(Word { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn join_to_num(tokens: &mut [Token], new_end: usize) -> bool {
    if let Some(Num { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn join_op(tokens: &mut [Token], new_end: usize) -> bool {
    if let Some(Operator { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn clean(st: &str) -> String {
    st.replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
}
fn clean_char(st: char) -> String {
    match st {
        '\n' => String::from("\\n"),
        '\t' => String::from("\\t"),
        '\r' => String::from("\\r"),
        _ => st.to_string()
    }
}

fn is_mut_pointer(tokens: &[Token], solid_tokens: &[SolidToken], pos: usize, input_code: &str) -> bool {
    if pos == 0 { return false }
    let Some(SolidToken::UnaryOperator(OperatorType::Pointer)) = solid_tokens.last() else {
        return false
    };
    if let Word {start, end, ..} = tokens[pos] {
        return &slice(input_code, start, end) == "mut"
    }
    false
}

#[inline]
fn slice(input_code: &str, start: usize, end: usize) -> String {
    input_code.chars().skip(start).take(end - start).collect()
}