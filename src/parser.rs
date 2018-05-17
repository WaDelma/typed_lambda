use std::fmt;

use Ident;
use lexer::{Token, TokenType};

#[derive(PartialEq)]
pub enum Expr {
    Wut,
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
            Wut => fmt.write_str("wut"),
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

// TODO: Track positions
pub fn parse<I: Iterator<Item=Token>>(mut tokens: &mut I) -> Expr {
    use lexer::TokenType::*;
    let mut params = vec![];
    let mut value = None;
    let mut state = State::General;
    while let Some(token) = tokens.next() {
        match state {
            State::General => match token.token {
                Error(e) => return Expr::Error(e.into()),
                Lambda => state = State::Lambda(1),
                BracketStart => state = State::App(1),
                Let => state = State::Let(1),
                Ident(i) => return Expr::Var(i),
                _ => panic!("Invalid token: {:?}", token),
            },
            State::Lambda(n) => match token.token {
                Ident(ref i) if n == 1 => params.push(i.clone()),
                Dot if n == 1 => state = State::Lambda(2),
                // TODO: use t
                ref t if n == 2 => return Expr::Abs(params, Box::new(parse(tokens))),
                _ => panic!("Invalid token: {:?}", token),
            },
            State::App(n) => match token.token {
              // TODO: Application
              _ => {},  
            },
            State::Let(n) => match token.token {
                Ident(ref i) if n == 1 => {
                    params.push(i.clone());
                    state = State::Let(2);
                }
                Equals if n == 2 => state = State::Let(3),
                // TODO: use t
                ref t if n == 3 => {
                    value = Some(parse(tokens));
                    state = State::Let(4);
                },
                In if n == 4 => state = State::Let(5),
                // TODO: use t
                ref t if n == 5 => return Expr::Let(params.drain(0..1).next().unwrap(), Box::new(value.unwrap()), Box::new(parse(tokens))),
                _ => panic!("Invalid token: {:?}", token),
            },
        }
    }
    Expr::Wut
    // Expr::Error(ParseError::MissingTokens)
}

#[test]
fn identity_unicode() {
    use self::Expr::*;
    use lexer::lexer;
    assert_eq!(
        parse(&mut lexer("λx.x".chars())),
        Abs(
            vec!["x".into()],
            Box::new(Var(
                "x".into()
            ))
        )
    )
}
