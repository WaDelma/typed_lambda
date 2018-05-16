use std::fmt;

use unicode_xid::UnicodeXID;

#[derive(PartialEq)]
pub struct Token {
    token: TokenType,
    line: usize,
    from_column: usize,
    to_column: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt,
            "{token:?} at {line}:{from}→{to}",
            token=self.token,
            line=self.line,
            from=self.from_column,
            to=self.to_column
        )
    }
}

impl Token {
    fn new(token: TokenType, line: usize, from_column: usize, to_column: usize) -> Self {
        Token {
            token,
            line,
            from_column,
            to_column,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Error(LexError),
    Lambda,
    BracketStart,
    BracketEnd,
    Dot,
    Let,
    Equals,
    In,
    Ident(String)
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    UnknownCharacter(char),
    InvalidIdentifier {
        identifier: String,
        column: usize,
    },
}

pub fn lexer<I: IntoIterator<Item=char>>(chars: I) -> impl Iterator<Item=Token> {
    Tokens {
        iter: chars.into_iter(),
        line: 0,
        column: 0,
        prev: None,
    }
}

struct Tokens<I> {
    iter: I,
    line: usize,
    column: usize,
    prev: Option<char>,
}

enum State {
    General,
    Ident,
    Let(u8),
    In(u8)
}

impl<I> Tokens<I> {
    fn tok(&self, token: TokenType) -> Token {
        Token {
            token,
            line: self.line,
            from_column: self.column - 1,
            to_column: self.column,
        }
    }
}

impl<I: Iterator<Item=char>> Iterator for Tokens<I> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        use self::TokenType::*;
        use self::LexError::*;
        let mut buffer = String::new();
        let mut state = State::General;
        if let Some(c) = self.prev.take() {
            match c {
                '(' => return Some(self.tok(BracketStart)),
                ')' => return Some(self.tok(BracketEnd)),
                '.' => return Some(self.tok(Dot)),
                '=' => return Some(self.tok(Equals)),
                'λ' | '\\' => return Some(self.tok(Lambda)),
                _ => unreachable!("Previous character should be one of '(', ')', '.', '=', '\\', 'λ'."),
            }
        }
        while let Some(c) = self.iter.next() {
            self.column += 1;
            match state {
                State::General => {
                    let tok = match c {
                        '(' => BracketStart,
                        ')' => BracketEnd,
                        '.' => Dot,
                        '=' => Equals,
                        'λ' | '\\' => Lambda,
                        'i' => {
                            state = State::In(1);
                            continue;
                        }
                        'l' => {
                            state = State::Let(1);
                            continue;
                        }
                        c if UnicodeXID::is_xid_start(c) =>  {
                            buffer.push(c);
                            state = State::Ident;
                            continue;
                        },
                        '\n' => {
                            self.line += 1;
                            self.column = 0;
                            continue;
                        },
                        c if c.is_whitespace() => {
                            continue;
                        },
                        c => return Some(self.tok(Error(UnknownCharacter(c)))),
                    };
                    return Some(self.tok(tok));
                },
                State::Ident => match c {
                    c if UnicodeXID::is_xid_continue(c) => buffer.push(c),
                    c if c.is_whitespace() => {
                        let column = self.column - 1;
                        let from_column = column - buffer.len();
                        let tok = Token::new(Ident(buffer), self.line, from_column, column);
                        if c == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }
                        return Some(tok);
                    },
                    '(' | ')' | '.' | '=' | 'λ' | '\\' => {
                        self.prev = Some(c);
                        let column = self.column - 1;
                        let from_column = column - buffer.len();
                        return Some(Token::new(Ident(buffer), self.line, from_column, column));
                    },
                    c => {
                        buffer.push(c);
                        let column = self.column - 1;
                        let mut column_to_end = self.column;
                        while let Some(c) = self.iter.next() {
                            self.column += 1;
                            column_to_end += 1;
                            buffer.push(c);
                            match c {
                                '(' | ')' | '.' | '=' | 'λ' | '\\' => {
                                    self.prev = Some(c);
                                    break;
                                },
                                c if c.is_whitespace() => {
                                    if c == '\n' {
                                        self.line += 1;
                                        self.column = 0;
                                    }
                                    break;
                                },
                                _ => {},
                            }
                        }
                        let from_column = column_to_end - (buffer.len() - 1);
                        return Some(Token::new(Error(InvalidIdentifier { column, identifier: buffer }), self.line, from_column, column_to_end));
                    },
                },
                State::Let(n) => match c {
                    'e' if n == 1 => state = State::Let(2),
                    't' if n == 2 => state = State::Let(3),
                    c if n == 3 => if c.is_whitespace() {
                        let column = self.column - 1;
                        let tok = Token::new(Let, self.line, column - 3, column);
                        if c == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }
                        return Some(tok);
                    } else {
                        buffer.push_str("let");
                        state = State::Ident;
                    },
                    c => panic!("Unknown character: {} at {}:{}", c, self.line, self.column),
                }
                State::In(n) => match c {
                    'n' if n == 1 => state = State::In(2),
                    c if n == 2 => if c.is_whitespace() {
                        let column = self.column - 1;
                        let tok = Token::new(In, self.line, column - 2, column);
                        if c == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }
                        return Some(tok);
                    } else {
                        buffer.push_str("in");
                        state = State::Ident;
                    },
                    c => panic!("Unknown character: {} at {}:{}", c, self.line, self.column),
                }
            }
        }
        match state {
            State::General => None,
            State::Ident => Some(self.tok(Ident(buffer))),
            State::Let(n) => Some(if n == 3 {
                self.tok(Let)
            } else {
                Token::new(Ident("let"[0..n as usize].into()), self.line, self.column - n as usize, self.column)
            }),
            State::In(n) => Some(if n == 2 {
                self.tok(In)
            } else {
                self.tok(Ident("i".into()))
            }),
            _ => panic!("Input ended too early."),
        }
    }
}

#[test]
fn identity_unicode() {
    use self::TokenType::*;
    assert_eq!(
        lexer("λx.x".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Lambda, 0, 0, 1),
            Token::new(Ident("x".into()), 0, 1, 2),
            Token::new(Dot, 0, 2, 3),
            Token::new(Ident("x".into()), 0, 3, 4),
        ]
    )
}

#[test]
fn identity_ascii() {
    use self::TokenType::*;
    assert_eq!(
        lexer("\\x.x".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Lambda, 0, 0, 1),
            Token::new(Ident("x".into()), 0, 1, 2),
            Token::new(Dot, 0, 2, 3),
            Token::new(Ident("x".into()), 0, 3, 4),
        ]
    )
}

#[test]
fn let_abstraction() {
    use self::TokenType::*;
    assert_eq!(
        lexer("let x=y in z".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Let, 0, 0, 3),
            Token::new(Ident("x".into()), 0, 4, 5),
            Token::new(Equals, 0, 5, 6),
            Token::new(Ident("y".into()), 0, 6, 7),
            Token::new(In, 0, 8, 10),
            Token::new(Ident("z".into()), 0, 11, 12)
        ]
    )
}

#[test]
fn splitting_lines() {
    use self::TokenType::*;
    assert_eq!(
        lexer("λx.\nλx.x".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Lambda, 0, 0, 1),
            Token::new(Ident("x".into()), 0, 1, 2),
            Token::new(Dot, 0, 2, 3),
            Token::new(Lambda, 1, 0, 1),
            Token::new(Ident("x".into()), 1, 1, 2),
            Token::new(Dot, 1, 2, 3),
            Token::new(Ident("x".into()), 1, 3, 4)
        ]
    )
}

#[test]
fn ends_with_i() {
    use self::TokenType::*;
    use self::LexError::*;
    assert_eq!(
        lexer("i".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("i".into()), 0, 0, 1),
        ]
    )
}

#[test]
fn ends_with_l() {
    use self::TokenType::*;
    use self::LexError::*;
    assert_eq!(
        lexer("l".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("l".into()), 0, 0, 1),
        ]
    )
}

#[test]
fn ends_with_le() {
    use self::TokenType::*;
    use self::LexError::*;
    assert_eq!(
        lexer("le".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("le".into()), 0, 0, 2),
        ]
    )
}

#[test]
fn unknown_char() {
    use self::TokenType::*;
    use self::LexError::*;
    assert_eq!(
        lexer("🤔".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Error(UnknownCharacter('🤔')), 0, 0, 1),
        ]
    )
}

#[test]
fn invalid_identifier() {
    use self::TokenType::*;
    use self::LexError::*;
    assert_eq!(
        lexer("a§d".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Error(InvalidIdentifier {
                identifier: "a§d".into(),
                column: 1,
            }), 0, 0, 3)
        ]
    )
}