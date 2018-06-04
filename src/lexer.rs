use std::fmt;

use unicode_xid::UnicodeXID;

#[derive(PartialEq)]
pub struct Token {
    pub token: TokenType,
    pub line: usize,
    pub from_column: usize,
    pub to_column: usize,
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
    pub fn new(token: TokenType, line: usize, from_column: usize, to_column: usize) -> Self {
        Token {
            token,
            line,
            from_column,
            to_column,
        }
    }

    pub fn from(&self) -> (usize, usize) {
        (self.line, self.from_column)
    }

    pub fn to(&self) -> (usize, usize) {
        (self.line, self.to_column)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl State {
    fn state_cons(&self) -> (fn(u8) -> State) {
        use self::State::*;
        match self {
            Let(_) => Let,
            In(_) => In,
            _ => panic!("Not keyword"),
        }
    }
    fn token_type(&self) -> TokenType {
        use self::State::*;
        match self {
            Let(_) => TokenType::Let,
            In(_) => TokenType::In,
            _ => panic!("Doesn't correspond to token type"),
        }
    }
    fn keyword(&self) -> &'static str {
        use self::State::*;
        match self {
            Let(_) => "let",
            In(_) => "in",
            _ => panic!("Not keyword"),
        }
    }
}

fn is_punctuation(c: char) -> bool {
    match c {
        '(' | ')' | '.' | '=' | 'λ' | '\\' => true,
        _ => false,
    }
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

    fn check_linebreak(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        }
    }

    fn lex_keyword(
        &mut self,
        state: &mut State,
        buffer: &mut String,
        c: char,
        n: u8,
    ) -> Option<Token> {
        let keyword = state.keyword();
        let m = n as usize;
        if m < keyword.len() {
            if keyword.as_bytes()[m] == c as u8 {
                *state = (state.state_cons())(n + 1);
                return None;
            }
        } else if c.is_whitespace() || is_punctuation(c) {
            let column = self.column - 1;
            let tok = Token::new(state.token_type(), self.line, column - m, column);
            if is_punctuation(c) {
                self.prev = Some(c);
                self.column -= 1;
            }
            self.check_linebreak(c);
            return Some(tok);
        }
        self.prev = Some(c);
        self.column -= 1;
        buffer.push_str(&keyword[0..m]);
        *state = State::Ident;
        return None;
    }
}

impl<I: Iterator<Item=char>> Iterator for Tokens<I> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        use self::TokenType::*;
        use self::LexError::*;
        let mut buffer = String::new();
        let mut state = State::General;
        while let Some(c) =  self.prev.take().or_else(|| self.iter.next()) {
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
                        c if c.is_whitespace() => {
                            self.check_linebreak(c);
                            continue
                        },
                        c => Error(UnknownCharacter(c)),
                    };
                    return Some(self.tok(tok));
                },
                State::Ident => match c {
                    c if UnicodeXID::is_xid_continue(c) => buffer.push(c),
                    c if c.is_whitespace() => {
                        let column = self.column - 1;
                        let from_column = column - buffer.len();
                        let tok = Token::new(Ident(buffer), self.line, from_column, column);
                        self.check_linebreak(c);
                        return Some(tok);
                    },
                    c if is_punctuation(c) => {
                        self.prev = Some(c);
                        self.column -= 1;
                        let from_column = self.column - buffer.len();
                        return Some(Token::new(Ident(buffer), self.line, from_column, self.column));
                    },
                    c => {
                        buffer.push(c);
                        let err_column = self.column - 1;
                        while let Some(c) = self.iter.next() {
                            match c {
                                c if is_punctuation(c) => {
                                    self.prev = Some(c);
                                    break;
                                },
                                c if c.is_whitespace() => {
                                    self.check_linebreak(c);
                                    break;
                                },
                                _ => {
                                    self.column += 1;
                                    buffer.push(c);
                                },
                            }
                        }
                        let from_column = self.column - (buffer.len() - 1);
                        let err = Error(InvalidIdentifier { column: err_column, identifier: buffer });
                        return Some(Token::new(err, self.line, from_column, self.column));
                    },
                },
                State::Let(n) | State::In(n) => {
                    let keyword = self.lex_keyword(&mut state, &mut buffer, c, n);
                    if keyword.is_some() {
                        return keyword;
                    }
                },
            }
        }
        match state {
            State::General => None,
            State::Ident => {
                let from_column = self.column - buffer.len();
                Some(Token::new(Ident(buffer), self.line, from_column, self.column))
            },
            State::Let(n) | State::In(n) => Some(if n == state.keyword().len() as u8 {
                self.tok(state.token_type())
            } else {
                let ident = Ident(state.keyword()[0..n as usize].into());
                Token::new(ident, self.line, self.column - n as usize, self.column)
            }),
        }
    }
}

#[test]
fn identity_abstraction_unicode() {
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
fn identity_abstraction_ascii() {
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
fn application() {
    use self::TokenType::*;
    assert_eq!(
        lexer("(abc xyz)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(BracketStart, 0, 0, 1),
            Token::new(Ident("abc".into()), 0, 1, 4),
            Token::new(Ident("xyz".into()), 0, 5, 8),
            Token::new(BracketEnd, 0, 8, 9),
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
fn keywords() {
    use self::TokenType::*;
    assert_eq!(
        lexer("let)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Let, 0, 0, 3),
            Token::new(BracketEnd, 0, 3, 4),
        ]
    );
    assert_eq!(
        lexer("in)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(In, 0, 0, 2),
            Token::new(BracketEnd, 0, 2, 3),
        ]
    );
}

#[test]
fn prefix_of_keyword() {
    use self::TokenType::*;
    assert_eq!(
        lexer("i".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("i".into()), 0, 0, 1),
        ]
    );
    assert_eq!(
        lexer("i ".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("i".into()), 0, 0, 1),
        ]
    );
    assert_eq!(
        lexer("i)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("i".into()), 0, 0, 1),
            Token::new(BracketEnd, 0, 1, 2),
        ]
    );
    assert_eq!(
        lexer("l".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("l".into()), 0, 0, 1),
        ]
    );
    assert_eq!(
        lexer("l ".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("l".into()), 0, 0, 1),
        ]
    );
    assert_eq!(
        lexer("l)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("l".into()), 0, 0, 1),
            Token::new(BracketEnd, 0, 1, 2),
        ]
    );
    assert_eq!(
        lexer("le".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("le".into()), 0, 0, 2),
        ]
    );
    assert_eq!(
        lexer("le ".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("le".into()), 0, 0, 2),
        ]
    );
    assert_eq!(
        lexer("le)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("le".into()), 0, 0, 2),
            Token::new(BracketEnd, 0, 2, 3),
        ]
    );
    assert_eq!(
        lexer("inn".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("inn".into()), 0, 0, 3),
        ]
    );
    assert_eq!(
        lexer("inn ".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("inn".into()), 0, 0, 3),
        ]
    );
    assert_eq!(
        lexer("lett".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("lett".into()), 0, 0, 4),
        ]
    );
    assert_eq!(
        lexer("lett ".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Ident("lett".into()), 0, 0, 4),
        ]
    );
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
    );
    assert_eq!(
        lexer("a§d ".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(Error(InvalidIdentifier {
                identifier: "a§d".into(),
                column: 1,
            }), 0, 0, 3)
        ]
    );
    assert_eq!(
        lexer("(a§d)".chars()).collect::<Vec<_>>(),
        vec![
            Token::new(BracketStart, 0, 0, 1),
            Token::new(Error(InvalidIdentifier {
                identifier: "a§d".into(),
                column: 2,
            }), 0, 1, 4),
            Token::new(BracketEnd, 0, 4, 5),
        ]
    );
}