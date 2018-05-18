use std::fmt;

use Ident;
use lexer::{Token, TokenType};

#[derive(PartialEq)]
pub struct Expr {
    expr: ExprType,
    from: (usize, usize),
    to: (usize, usize),
}

impl Expr {
    pub fn new(expr: ExprType, from: (usize, usize), to: (usize, usize)) -> Self {
        Expr {
            expr,
            from,
            to,
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "{:?} at {from_line}:{from_column}→{to_line}:{to_column}",
            self.expr,
            from_line=self.from.0,
            from_column=self.from.1,
            to_line=self.to.0,
            to_column=self.to.1,
        )
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.expr)
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Error(ParseError),
    Var(Ident),
    App(Box<Expr>, Box<Expr>),
    Abs(Vec<Ident>, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

impl fmt::Display for ExprType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprType::*;
        match *self {
            Error(ref e) => write!(fmt, "{:?}", e),
            Var(ref ident) => fmt.write_str(ident),
            App(ref lhs, ref rhs) => write!(fmt, "({}) {}", lhs, rhs),
            Abs(ref ident, ref rhs) => write!(fmt, "λ{}.{}", ident.join(" "), rhs),
            Let(ref ident, ref value, ref target) => write!(fmt, "let {} = {} in {}", ident, value, target),
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
    Bracket(u8),
    Lambda(u8),
    Let(u8),
}

pub struct Parser {
    prev: Option<Token>
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            prev: None,
        }
    }
}

impl Parser {
    pub fn parse<I: Iterator<Item=Token>>(&mut self, tokens: &mut I) -> Expr {
        use lexer::TokenType::*;
        let mut params = vec![];
        let mut value = vec![];
        let mut expr = None::<Expr>;
        let mut state = State::General;
        let mut start = None;
        while let Some(token) = self.prev.take().or_else(|| tokens.next()) {
            let from = token.from();
            let to = token.to();
            match state {
                State::General => match token.token {
                    Error(err) => {
                        let err = Expr::new(ExprType::Error(err.into()), from, to);
                        expr = Some(if let Some(e) = expr.take() {
                            let from = e.from;
                            let to = e.to;
                            Expr::new(
                                ExprType::App(Box::new(e), Box::new(err)),
                                from,
                                to,
                            )
                        } else {
                            err
                        });
                    },
                    Lambda => {
                        start = Some(from);
                        state = State::Lambda(1)
                    },
                    BracketStart => {
                        start = Some(from);
                        state = State::Bracket(1);
                    },
                    Let => {
                        start = Some(from);
                        state = State::Let(1);
                    },
                    BracketEnd => if let Some(e) = expr {
                        self.prev = Some(token);
                        return e;
                    } else {
                        panic!("Invalid token: {:?}", token);
                    },
                    In => if let Some(e) = expr {
                        self.prev = Some(token);
                        return e;
                    } else {
                        panic!("Invalid token: {:?}", token);
                    },
                    Ident(i) => {
                        let var = Expr::new(ExprType::Var(i), from, to);
                        expr = Some(if let Some(e) = expr.take() {
                            let (from, to) = (e.from, var.to);
                            Expr::new(
                                ExprType::App(Box::new(e), Box::new(var)),
                                from,
                                to,
                            )
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
                        let from = start.unwrap();
                        self.prev = Some(token);
                        let body = Box::new(self.parse(tokens));
                        let abs = Expr::new(ExprType::Abs(params, body), from, to);
                        params = vec![];
                        expr = Some(if let Some(e) = expr.take() {
                            let (from, to) = (e.from, abs.to);
                            Expr::new(
                                ExprType::App(Box::new(e), Box::new(abs)),
                                from,
                                to,
                            )
                        } else {
                            abs
                        });
                    },
                    _ => unreachable!("State cannot be reached"),
                },
                State::Bracket(n) => match n {
                    1 => {
                        self.prev = Some(token);
                        let mut ex = self.parse(tokens);
                        expr = Some(if let Some(e) = expr.take() {
                            let (from, to) = (start.unwrap(), ex.to);
                            Expr::new(
                                ExprType::App(Box::new(e), Box::new(ex)),
                                from,
                                to,
                            )
                        } else {
                            ex.from.1 -= 1;
                            ex.to.1 += 1;
                            ex
                        });
                        state = State::Bracket(2);
                    }
                    2 => match token.token {
                        BracketEnd => state = State::General,
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
                        let from = start.unwrap();
                        self.prev = Some(token);
                        let let_ = Expr::new(
                            ExprType::Let(
                                params.pop().unwrap(),
                                Box::new(value.pop().unwrap()),
                                Box::new(self.parse(tokens))
                            ),
                            from,
                            to,
                        );
                        expr = Some(if let Some(e) = expr {
                            let (from, to) = (e.from, let_.to);
                            Expr::new(
                                ExprType::App(Box::new(e), Box::new(let_)),
                                from,
                                to,
                            )
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
            Expr::new(
                ExprType::Error(ParseError::MissingTokens),
                (0, 0),
                (0, 0)
            )
        }
    }
}

#[test]
fn identity_abstraction() {
    use self::ExprType::*;
    use lexer::lexer;
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("λx.x".chars())),
        Expr::new(Abs(
            vec!["x".into()],
            Box::new(Expr::new(Var(
                "x".into()
            ), (0, 3), (0, 4)))
        ), (0, 0), (0, 4))
    )
}

#[test]
fn application() {
    use self::ExprType::*;
    use lexer::lexer;
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("a b c".chars())),
        Expr::new(App(
            Box::new(Expr::new(App(
                Box::new(Expr::new(Var(
                    "a".into(),
                ), (0, 0), (0, 1))),
                Box::new(Expr::new(Var(
                    "b".into(),
                ), (0, 2), (0, 3)))
            ), (0, 0), (0, 3))),
            Box::new(Expr::new(Var(
                "c".into(),
            ), (0, 4), (0, 5))),
        ), (0, 0), (0, 5))
    );
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("(d e) f".chars())),
        Expr::new(App(
            Box::new(Expr::new(App(
                Box::new(Expr::new(Var(
                    "d".into(),
                ), (0, 1), (0, 2))),
                Box::new(Expr::new(Var(
                    "e".into(),
                ), (0, 3), (0, 4)))
            ), (0, 0), (0, 5))),
            Box::new(Expr::new(Var(
                "f".into(),
            ), (0, 6), (0, 7))),
        ), (0, 0), (0, 7))
    );
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("g (h) j".chars())),
        Expr::new(App(
            Box::new(Expr::new(App(
                Box::new(Expr::new(Var(
                    "g".into(),
                ), (0, 0), (0, 1))),
                Box::new(Expr::new(Var(
                    "h".into(),
                ), (0, 3), (0, 4)))
            ), (0, 0), (0, 5))),
            Box::new(Expr::new(Var(
                "j".into(),
            ), (0, 6), (0, 7))),
        ), (0, 0), (0, 7))
    );
}

#[test]
fn let_abstraction() {
    use self::ExprType::*;
    use lexer::lexer;
    let mut parser = Parser::new();
    assert_eq!(
        parser.parse(&mut lexer("let x = y in z".chars())),
        Expr::new(Let(
            "x".into(),
            Box::new(Expr::new(Var(
                "y".into(),
            ), (0, 8), (0, 9))),
            Box::new(Expr::new(Var(
                "z".into(),
            ), (0, 13), (0, 14))),
        ), (0, 0), (0, 14))
    )
}