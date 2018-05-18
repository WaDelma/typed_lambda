use std::fmt;

use Ident;
use lexer::{Token, TokenType};

#[derive(PartialEq)]
pub enum Expr {
    Error(ParseError),
    Var(Ident),
    App(Box<Expr>, Box<Expr>),
    Abs(Vec<Ident>, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

impl fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Error(ref e) => write!(fmt, "{:?}", e),
            Var(ref ident) => fmt.write_str(ident),
            App(ref lhs, ref rhs) => write!(fmt, "({:?}) {:?}", lhs, rhs),
            Abs(ref ident, ref rhs) => write!(fmt, "λ{}.{:?}", ident.join(" "), rhs),
            Let(ref ident, ref value, ref target) => write!(fmt, "let {} = {:?} in {:?}", ident, value, target),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    LexError(::lexer::LexError),
    MissingTokens,
}

impl From<::lexer::LexError> for ParseError {
    fn from(e: ::lexer::LexError) -> Self {
        ParseError::LexError(e)
    }
}

enum State {
    General,
    Lambda(u8),
    Let(u8),
    App(u8),
}

struct Parser {
    prev: Option<Token>
}

impl Parser {
    fn new() -> Parser {
        Parser {
            prev: None,
        }
    }
}

impl Parser {
    // TODO: Track positions
    pub fn parse<I: Iterator<Item=Token>>(&mut self, tokens: &mut I) -> Expr {
        use lexer::TokenType::*;
        let mut params = vec![];
        let mut value = vec![];
        let mut expr = None;
        let mut state = State::General;
        while let Some(token) = self.prev.take().or_else(|| tokens.next()) {
            match state {
                State::General => match token.token {
                    Error(err) => {
                        let err = Expr::Error(err.into());
                        expr = Some(if let Some(e) = expr.take() {
                            Expr::App(Box::new(e), Box::new(err))
                        } else {
                            err
                        });
                    },
                    Lambda => state = State::Lambda(1),
                    BracketStart => state = State::App(1),
                    BracketEnd => if let Some(e) = expr {
                        self.prev = Some(token);
                        return e;
                    } else {
                        panic!("Invalid token: {:?}", token);
                    },
                    Let => state = State::Let(1),
                    Ident(i) => {
                        let var = Expr::Var(i);
                        expr = Some(if let Some(e) = expr.take() {
                            Expr::App(Box::new(e), Box::new(var))
                        } else {
                            var
                        });
                    },
                    _ => panic!("Invalid token: {:?}", token),
                },
                State::Lambda(n) => match n {
                    1 => match token.token {
                        Ident(i) => params.push(i),
                        Dot => state = State::Lambda(2),
                        _ => panic!("Invalid token: {:?}", token),
                    }
                    2 => {
                        // TODO: What am I doing with token here?
                        self.prev = Some(token);
                        let lambda = self.parse(tokens);
                        let abs = Expr::Abs(params, Box::new(lambda));
                        params = vec![];
                        expr = Some(if let Some(e) = expr.take() {
                            Expr::App(Box::new(e), Box::new(abs))
                        } else {
                            abs
                        });
                    },
                    _ => unreachable!("State cannot be reached"),
                },
                State::App(n) => match n {
                    1 => {
                        self.prev = Some(token);
                        value.push(self.parse(tokens));
                        state = State::App(2);
                    },
                    2 => {
                        self.prev = Some(token);
                        value.push(self.parse(tokens));
                        state = State::App(3);
                    },
                    3 => match token.token {
                        BracketEnd => {
                            let mut value = value.drain(..);
                            let app = Expr::App(Box::new(value.next().unwrap()), Box::new(value.next().unwrap()));
                            expr = Some(if let Some(e) = expr.take() {
                                Expr::App(Box::new(e), Box::new(app))
                            } else {
                                app
                            });
                        },
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    _ => unreachable!("State cannot be reached"),
                },
                State::Let(n) => match n {
                    1 => match token.token {
                        Ident(i) => {
                            params.push(i);
                            state = State::Let(2);
                        },
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    2 => match token.token {
                        Equals if n == 2 => state = State::Let(3),
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    3 => {
                        self.prev = Some(token);
                        value.push(self.parse(tokens));
                        state = State::Let(4);
                    },
                    4 => match token.token {
                        In => state = State::Let(5),
                        _ => panic!("Invalid token: {:?}", token),
                    }
                    5 => {
                        self.prev = Some(token);
                        let let_ = Expr::Let(params.drain(..).next().unwrap(), Box::new(value.drain(..).next().unwrap()), Box::new(self.parse(tokens)));
                        expr = Some(if let Some(e) = expr {
                            Expr::App(Box::new(e), Box::new(let_))
                        } else {
                            let_
                        });
                    },
                    _ => unreachable!("State cannot be reached"),
                },
            }
        }
        if let Some(e) = expr {
            e
        } else {
            Expr::Error(ParseError::MissingTokens)
        }
    }
}

#[test]
fn identity_abstraction() {
    use self::Expr::*;
    use lexer::lexer;
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("λx.x".chars())),
        Abs(
            vec!["x".into()],
            Box::new(Var(
                "x".into()
            ))
        )
    )
}

#[test]
fn application() {
    use self::Expr::*;
    use lexer::lexer;
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("a b c".chars())),
        App(
            Box::new(App(
                Box::new(Var(
                    "a".into(),
                )),
                Box::new(Var(
                    "b".into(),
                ))
            )),
            Box::new(Var(
                "c".into(),
            )),
        )
    );
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("(a b) c".chars())),
        App(
            Box::new(App(
                Box::new(Var(
                    "a".into(),
                )),
                Box::new(Var(
                    "b".into(),
                ))
            )),
            Box::new(Var(
                "c".into(),
            )),
        )
    );
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("a (b) c".chars())),
        App(
            Box::new(App(
                Box::new(Var(
                    "a".into(),
                )),
                Box::new(Var(
                    "b".into(),
                ))
            )),
            Box::new(Var(
                "c".into(),
            )),
        )
    );
}

#[test]
fn let_abstraction() {
    use self::Expr::*;
    use lexer::lexer;
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("let x = y in z".chars())),
        Let(
            "x".into(),
            Box::new(Var(
                "y".into(),
            )),
            Box::new(Var(
                "z".into(),
            )),
        )
    )
}