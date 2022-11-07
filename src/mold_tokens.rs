// works?
use std::collections::HashSet;
use crate::mold_tokens::Token::{Brace, Bracket, Colon, Comma, Num, NewLine, Operator, Parenthesis, Period, Tab, Word, Str, Char};


#[derive(Debug)]
pub enum IsOpen { True, False }


#[derive(Debug)]
pub enum Token {
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

// lazy_static! {
//     static ref VALID_LONG_OPERATORS: HashSet<&'static str> = HashSet::from(
//         ["//", "**", "==", "!=", "/=", "//=", "+=", "-=", "*=", "%=", "^=", "&=", "**="]
//     );
// }

pub fn tokenize(input_code: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut skip = 0;
    let chars: Vec<char> = input_code.chars().collect();
    for (i, c) in chars.iter().enumerate() {
        if skip > 0 {
            skip -= 1;
            continue
        }
        let token = match c {
            '+' | '-' | '*' | '&' | '^' | '%' | '!' | '|' | '/' | '=' => Operator {
                start: i, end: i + 1, is_spaced: false
            },
            '[' => Bracket(IsOpen::True),       ']' => Bracket(IsOpen::False),
            '{' => Brace(IsOpen::True),         '}' => Brace(IsOpen::False),
            '(' => Parenthesis(IsOpen::True),   ')' => Parenthesis(IsOpen::False),
            ' ' => {
                space_prev_token(&mut tokens);
                if is_tab(&chars, i) {  skip = 3;   Tab   } else { continue }
            },
            '\t' => Tab,   '\n' => NewLine,
            ':' => Colon,   ',' => Comma,   '.' => Period,

            '\'' => {
                if chars[i + 1] == '\\' {
                    skip = 3;
                    // todo tokens.push(Char())
                } else {
                    skip = 2;
                    tokens.push(Char(chars[i + 1]))
                }
                continue
            },
            // '"' => (),

            '0'..='9' => Num { start: i, end: i + 1 },
            _ => Word { start: i, end: i + 1, is_spaced: false }
        };
        match token {
            Operator { is_spaced: false, .. } if join_op(&mut tokens, i + 1) => (),
            Word { is_spaced: false, .. } if join_to_word(&mut tokens, i + 1) => (),
            Num {..} if join_int(&mut tokens, i + 1) => (),
            Period if join_int(&mut tokens, i + 1) => (),
            _ => tokens.push(token)
        };
    }
    tokens
}

fn join_int(tokens: &mut Vec<Token>, new_end: usize) -> bool {
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