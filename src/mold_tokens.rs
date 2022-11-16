use std::fmt::{Display, Formatter};
use std::fmt::Write;
// use std::collections::HashSet;
use crate::mold_tokens::Token::{
    Brace, Bracket, Colon, Comma, Num, NewLine, Operator, Parenthesis, Period, Tab, Word, Str, Char
};


#[derive(Debug, Clone)]
pub enum IsOpen { True, False }


#[derive(Debug)]
enum Token {
    Brace(IsOpen), Bracket(IsOpen), Parenthesis(IsOpen),
    Word {
        start: usize,
        end: usize,
        is_spaced: bool
    },
    Str {
        start: usize,
        end: usize,
    },
    Char(char),
    Num {
        start: usize,
        end: usize,
    },
    Operator {
        start: usize,
        end: usize,
        is_spaced: bool
    },
    Colon, Comma, Period,
    Tab, NewLine
}

#[derive(Debug, Clone)]
pub enum SolidToken {
    Brace(IsOpen), Bracket(IsOpen), Parenthesis(IsOpen),
    Word(String),
    Str(String), Char(char),
    Int(String), // Float(f32),
    Operator(OperatorType),
    Colon, Comma, Period,
    Tab, NewLine,
    Def, Class, Enum, Struct,
    If, Else, Elif,
    Match, Case, While, For,
    Break, Continue, Return, Pass,
}

#[derive(Debug, Clone)]
pub enum OperatorType {
    Eq, IsEq, Bigger, Smaller, NEq, BEq, SEq,
    Plus, Minus, Mul, Pow, Div, Mod, FloorDiv,
    PlusEq, MinusEq, MulEq, PowEq, DivEq, ModEq, FloorDivEq,
    Or, And, Xor, BinNot,
    OrEq, AndEq, XorEq,
    ShiftR, ShiftL,
    Returns
}
// todo is, in, not
// todo unary
impl Display for OperatorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
            OperatorType::Pow => "**",
            OperatorType::Div => "/",
            OperatorType::Mod => "%",
            OperatorType::FloorDiv=> "//",
            OperatorType::PlusEq => "+=",
            OperatorType::MinusEq => "-=",
            OperatorType::MulEq => "*=",
            OperatorType::PowEq => "**=",
            OperatorType::DivEq => "/=",
            OperatorType::ModEq => "%=",
            OperatorType::FloorDivEq => "//=",
            OperatorType::Or => "|",
            OperatorType::And => "&",
            OperatorType::Xor => "^",
            OperatorType::BinNot => "~",
            OperatorType::OrEq => "|=",
            OperatorType::AndEq => "&=",
            OperatorType::XorEq => "^=",
            OperatorType::ShiftL => "<<",
            OperatorType::ShiftR => ">>",
            OperatorType::Returns => "->"
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
            OperatorType::And => 60,
            OperatorType::Xor => 50,
            OperatorType::Or => 40,
            OperatorType::IsEq
            | OperatorType::Bigger
            | OperatorType::Smaller
            | OperatorType::NEq
            | OperatorType::BEq
            | OperatorType::SEq => 30,
            _ => -100
        }
    }
}

pub fn tokenize(input_code: String) -> Vec<SolidToken> {
    let mut tokens = Vec::new();
    let mut skip = 0;
    let mut is_str = false;
    let mut is_comment = false;
    let mut escaped = false;
    let mut open_braces = 0;
    let mut open_brackets = 0;
    let mut open_parentheses = 0;
    let chars: Vec<char> = input_code.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        if skip > 0 {
            skip -= 1;
            continue
        }
        if is_comment {
            if *c == '\n' {
                is_comment = false;
            } else {
                continue
            }
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
                is_comment = true;
                continue
            },
            ' ' => {
                space_prev_token(&mut tokens);
                if is_tab(&chars, i) {  skip = 3;   Tab   } else { continue }
            },
            '\t' => Tab,   '\n' => NewLine,
            ':' => Colon,   ',' => Comma,   '.' => Period,
            '0'..='9' => Num { start: i, end: i + 1 },
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
                make_char(&mut tokens, &mut skip, &chars, i);
                continue
            },
            '"' => {
                is_str = true;
                tokens.push(Str {
                    start: i, end: i + 1
                });
                continue
            },
            // </editor-fold>
            _ => Word { start: i, end: i + 1, is_spaced: false }
        };
        match token {
            Operator { is_spaced: false, .. } if join_op(&mut tokens, i + 1) => (),
            Word { is_spaced: false, .. } if join_to_word(&mut tokens, i + 1) => (),
            Num {..} if join_num(&mut tokens, i + 1) => (),
            Period if join_num(&mut tokens, i + 1) => (),
            _ => tokens.push(token)
        };
    }
    solidify_tokens(&tokens, input_code)
}

fn solidify_tokens(tokens: &Vec<Token>, input_code: String) -> Vec<SolidToken> {
    let mut res = Vec::with_capacity(tokens.len());
    for token in tokens {
        let st = match token {
            Brace(is_open) =>        SolidToken::Brace(is_open.clone()),
            Bracket(is_open) =>      SolidToken::Bracket(is_open.clone()),
            Parenthesis(is_open) =>  SolidToken::Parenthesis(is_open.clone()),
            Char(chr) => SolidToken::Char(chr.clone()),
            Word { start, end, .. } => {
                let st = &input_code[*start..*end];
                match st {
                    "def" => SolidToken::Def, "class" => SolidToken::Class,
                    "enum" => SolidToken::Enum, "struct" => SolidToken::Struct,
                    "if" => SolidToken::If, "else" => SolidToken::Else,
                    "elif" => SolidToken::Elif,
                    "match" => SolidToken::Match, "case" => SolidToken::Case,
                    "while" => SolidToken::While, "for" => SolidToken::For,
                    "break" => SolidToken::Break, "continue" => SolidToken::Continue,
                    "return" => SolidToken::Return, "pass" => SolidToken::Pass,
                    _ => SolidToken::Word(String::from(st))
                }
            },
            Str { start, end } => SolidToken::Str(
                String::from(&input_code[*start..*end])
            ),
            Operator { start, end, .. } => {
                // todo this is messy...
                let mut oper = String::new();
                for c in input_code.chars().skip(*start).take(*end-*start) {
                    if !matches!(str_to_op_type(&(oper.clone() + &c.to_string())), Some(_)) {
                        res.push(SolidToken::Operator(str_to_op_type(&oper).unwrap()));
                        oper = c.to_string()
                    } else {
                        write!(&mut oper, "{}", c).unwrap();
                    }
                }
                SolidToken::Operator(if let Some(op) = str_to_op_type(&oper) {
                    op
                } else {
                    panic!("invalid operator {}", &oper.parse::<String>().unwrap())
                })
            },
            // Num { start, end } => parse_num(&input_code[*start..*end]),
            Num { start, end } => SolidToken::Int(
                String::from(&input_code[*start..*end])
            ),
            Colon => SolidToken::Colon, Comma => SolidToken::Comma, Period => SolidToken::Period,
            Tab => SolidToken::Tab,     NewLine => SolidToken::NewLine
        };
        res.push(st);
    }
    res
}

// fn parse_num(num: &str) -> SolidToken {
//     if num.contains('.') {
//         SolidToken::Float(num.parse::<f32>().unwrap())
//     } else {
//         SolidToken::Int(num.parse::<i32>().unwrap())
//     }
// }

fn str_to_op_type(st: &str) -> Option<OperatorType> {
    Some(match st {
        "=" => OperatorType::Eq,        "==" => OperatorType::IsEq,
        ">" => OperatorType::Bigger,    "<" => OperatorType::Smaller,
        "!=" => OperatorType::NEq,  ">=" => OperatorType::BEq,      "<=" => OperatorType::SEq,
        "+" => OperatorType::Plus,  "-" => OperatorType::Minus,     "*" => OperatorType::Mul,
        "/" => OperatorType::Div,   "//" => OperatorType::FloorDiv,
        "**" => OperatorType::Pow,  "%" => OperatorType::Mod,
        "+=" => OperatorType::PlusEq,   "-=" => OperatorType::MinusEq,  "*=" => OperatorType::MulEq,
        "/=" => OperatorType::DivEq,    "//=" => OperatorType::FloorDivEq,
        "**=" => OperatorType::PowEq,   "%=" => OperatorType::ModEq,
        "|" => OperatorType::Or,    "&" => OperatorType::And,
        "^" => OperatorType::Xor,   "~" => OperatorType::BinNot,
        "|=" => OperatorType::OrEq, "&=" => OperatorType::AndEq,    "^=" => OperatorType::XorEq,
        "->" => OperatorType::Returns,
        _ => return None
    })
}

fn make_char(tokens: &mut Vec<Token>, skip: &mut i32, chars: &Vec<char>, i: usize) {
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

fn join_num(tokens: &mut Vec<Token>, new_end: usize) -> bool {
    if let Some(Num { end, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn is_tab(chars: &Vec<char>, i: usize) -> bool {
    chars.len() > i + 3 && chars[i + 1] == ' ' && chars[i + 2] == ' ' && chars[i + 3] == ' '
}

fn space_prev_token(tokens: &mut Vec<Token>) {
    if let Some(Operator { is_spaced, .. }) = tokens.last_mut() {
        *is_spaced = true;
    } else if let Some(Word { is_spaced, .. }) = tokens.last_mut() {
        *is_spaced = true;
    }
}

fn join_to_word(tokens: &mut Vec<Token>, new_end: usize) -> bool {
    if let Some(Word { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}

fn join_op(tokens: &mut Vec<Token>, new_end: usize) -> bool {
    if let Some(Operator { end, is_spaced: false, .. }) = tokens.last_mut() {
        *end = new_end;
        true
    } else { false }
}