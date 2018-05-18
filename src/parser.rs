use std::fmt;
use std::mem::replace;

use Ident;
use lexer::Token;

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
    Bracket(BracketState),
    Lambda(LambdaState),
    Let(LetState),
}

#[derive(Clone, Copy)]
enum BracketState {
    MiddleS,
    EndS,
}

#[derive(Clone, Copy)]
enum LambdaState {
    ParamS,
    BodyS,
}

#[derive(Clone, Copy)]
enum LetState {
    VarS,
    EqS,
    ValS,
    InS,
    ExprS,
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

fn chain_apps(expr: &mut Option<Expr>, value: Expr) {
    *expr = Some(if let Some(e) = expr.take() {
        let (from, to) = (e.from, value.to);
        let app = ExprType::App(Box::new(e), Box::new(value));
        Expr::new(app, from, to)
    } else {
        value
    });
}

impl Parser {
    pub fn parse<I: Iterator<Item=Token>>(&mut self, tokens: &mut I) -> Expr {
        use lexer::TokenType::*;
        use self::LetState::*;
        use self::LambdaState::*;
        use self::BracketState::*;
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
                    Error(err) => chain_apps(
                        &mut expr,
                        Expr::new(ExprType::Error(err.into()), from, to)
                    ),
                    Lambda => {
                        start = Some(from);
                        state = State::Lambda(LambdaState::ParamS)
                    },
                    BracketStart => {
                        start = Some(from);
                        state = State::Bracket(BracketState::MiddleS);
                    },
                    Let => {
                        start = Some(from);
                        state = State::Let(LetState::VarS);
                    },
                    BracketEnd | In => if let Some(e) = expr {
                        self.prev = Some(token);
                        return e;
                    } else {
                        panic!("Invalid token: {:?}", token);
                    },
                    Ident(i) => chain_apps(
                        &mut expr,
                        Expr::new(ExprType::Var(i), from, to)
                    ),
                    _ => panic!("Invalid token: {:?}", token),
                },
                State::Lambda(n) => match n {
                    ParamS => match token.token {
                        Ident(i) => params.push(i),
                        Dot => state = State::Lambda(BodyS),
                        _ => panic!("Invalid token: {:?}", token),
                    }
                    BodyS => {
                        self.prev = Some(token);
                        let body = Box::new(self.parse(tokens)); // TODO: Remove recursion
                        let abs = ExprType::Abs(replace(&mut params, vec![]), body);
                        chain_apps(&mut expr, Expr::new(abs, start.unwrap(), to));
                    }
                },
                State::Bracket(n) => match n {
                    MiddleS => {
                        self.prev = Some(token);
                        let mut ex = self.parse(tokens); // TODO: Remove recursion
                        ex.from.1 -= 1;
                        ex.to.1 += 1;
                        chain_apps(&mut expr, ex);
                        state = State::Bracket(EndS);
                    },
                    EndS => match token.token {
                        BracketEnd => state = State::General,
                        _ => panic!("Invalid token: {:?}", token),
                    },
                },
                State::Let(n) => match n {
                    VarS => match token.token {
                        Ident(i) => {
                            params.push(i);
                            state = State::Let(EqS);
                        },
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    EqS => match token.token {
                        Equals => state = State::Let(ValS),
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    ValS => {
                        self.prev = Some(token);
                        value.push(self.parse(tokens)); // TODO: Remove recursion
                        state = State::Let(InS);
                    },
                    InS => match token.token {
                        In => state = State::Let(ExprS),
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    ExprS => {
                        let from = start.unwrap();
                        self.prev = Some(token);
                        let let_ = ExprType::Let(
                            params.pop().unwrap(),
                            Box::new(value.pop().unwrap()),
                            Box::new(self.parse(tokens)) // TODO: Remove recursion
                        );
                        chain_apps(&mut expr, Expr::new(let_, from, to));
                    },
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