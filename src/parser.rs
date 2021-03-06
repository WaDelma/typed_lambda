use std::fmt;

use {Ident, Type};
use lexer::Token;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub expr: ExprType,
    pub from: (usize, usize),
    pub to: (usize, usize),
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


impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.expr)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprType {
    Error(ParseError),
    Var(Ident),
    App(Box<Expr>, Box<Expr>),
    Abs(Ident, Option<Type>, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

impl fmt::Display for ExprType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprType::*;
        match self {
            Error(e) => write!(fmt, "{:?}", e),
            Var(ident) => fmt.write_str(ident),
            App(lhs, rhs) => {
                fn apps(lhs: &ExprType, fmt: &mut fmt::Formatter) -> fmt::Result {
                    match lhs {
                        App(lhs, rhs) => {
                            apps(&lhs.expr, fmt)?;
                            match rhs.expr {
                                Var(_) => write!(fmt, " {}", rhs.expr),
                                _ => write!(fmt, " ({})", rhs.expr),
                            }
                        }
                        Var(_) => write!(fmt, "{}", lhs),
                        _ => write!(fmt, "({})", lhs)
                    }
                }
                apps(&lhs.expr, fmt)?;
                match rhs.expr {
                    App(..) => write!(fmt, " ({})", rhs.expr),
                    _ => write!(fmt, " {}", rhs.expr),
                }
            },
            Abs(ident, ty, rhs) => {
                write!(fmt, "λ{}", ident)?;
                if let Some(ty) = ty {
                    write!(fmt, ":{}", ty)?;
                }
                fn abss(rhs: &ExprType, fmt: &mut fmt::Formatter) -> fmt::Result {
                    match rhs {
                        Abs(ident, ty, rhs) => {
                            write!(fmt, " {}", ident)?;
                            if let Some(ty) = ty {
                                write!(fmt, ":{}", ty)?;
                            }
                            abss(&rhs.expr, fmt)
                        },
                        _ => write!(fmt, ".{}", rhs)
                    }
                }
                abss(&rhs.expr, fmt)
            },
            Let(ident, value, target) => write!(fmt, "let {} = {} in {}", ident, value.expr, target.expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    LexError(::lexer::LexError),
    MissingTokens,
}

impl From<::lexer::LexError> for ParseError {
    fn from(e: ::lexer::LexError) -> Self {
        ParseError::LexError(e)
    }
}

#[derive(Debug)]
enum State {
    General,
    Bracket(BracketState),
    Lambda(LambdaState),
    Let(LetState),
    TypeAnnotation,
}

#[derive(Debug, Clone, Copy)]
enum BracketState {
    MiddleS,
    EndS,
}

#[derive(Debug, Clone, Copy)]
enum LambdaState {
    ParamS(bool),
    BodyS,
}

#[derive(Debug, Clone, Copy)]
enum LetState {
    VarS,
    EqS,
    ValS,
    InS,
    ExprS,
}

pub enum Command {
    Bracket,
    Abs(Ident),
    Type(Type),
    LetEmpty(Ident),
    Let(Ident, Expr)
}

impl Command {
    fn is_abs(&self) -> bool {
        use self::Command::*;
        if let Abs(_) = self {
            true
        } else {
            false
        }
    }
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
        let mut commands = vec![];
        let mut expr = None::<Expr>;
        let mut state = State::General;
        let mut start = None;
        let mut to = (0, 0);
        while let Some(token) = self.prev.take().or_else(|| tokens.next()) {
            let from = token.from();
            to = token.to();
            match state {
                State::General => match token.token {
                    Error(err) => chain_apps(
                        &mut expr,
                        Expr::new(ExprType::Error(err.into()), from, to)
                    ),
                    Lambda => {
                        start = Some(from);
                        state = State::Lambda(ParamS(true));
                    },
                    BracketStart => {
                        start = Some(from);
                        state = State::Bracket(MiddleS);
                    },
                    Let => {
                        start = Some(from);
                        state = State::Let(VarS);
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
                    ParamS(fst) => match token.token {
                        Ident(i) => {
                            commands.push((Command::Abs(i), if fst {
                                start.unwrap()
                            } else {
                                from
                            }));
                            state = State::Lambda(ParamS(false));
                        },
                        Colon => {
                            if commands.last().map(|(c, _)| c.is_abs()).unwrap_or(false) {
                                state = State::TypeAnnotation;
                            } else {
                                panic!("Invalid type annotation");
                            }
                        },
                        Dot => state = State::Lambda(BodyS),
                        _ => panic!("Invalid token: {:?}", token),
                    }
                    BodyS => {
                        self.prev = Some(token);
                        let body = self.parse(tokens);
                        let to = body.to;
                        let mut abs = body;
                        let mut ty = None;
                        while let Some(c) = commands.pop() {
                            match c {
                                (Command::Type(t), _) => ty = Some(t),
                                (Command::Abs(i), mut from) => {
                                    let expr_ty = ExprType::Abs(i, ty.take(), Box::new(abs));
                                    abs = Expr::new(expr_ty, from, to);
                                },
                                _ => {
                                    commands.push(c);
                                    break;
                                }
                            }
                        }
                        chain_apps(&mut expr, abs);
                        state = State::General;
                    }
                },
                State::TypeAnnotation => {
                    match token.token {
                        Ident(i) => {
                            commands.push((Command::Type(i), from));
                            state = State::Lambda(ParamS(false));
                        },
                        _ => panic!("Invalid token: {:?}", token),
                    }
                }
                State::Bracket(n) => match n {
                    MiddleS => {
                        self.prev = Some(token);
                        let mut ex = self.parse(tokens);
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
                            commands.push((Command::LetEmpty(i), from));
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
                        let i = match commands.pop().unwrap().0 {
                            Command::LetEmpty(i) => i,
                            _ => unreachable!("There has to be LetEmpty command"),
                        };
                        commands.push((Command::Let(i, self.parse(tokens)), from));
                        state = State::Let(InS);
                    },
                    InS => match token.token {
                        In => state = State::Let(ExprS),
                        _ => panic!("Invalid token: {:?}", token),
                    },
                    ExprS => {
                        let from = start.unwrap();
                        self.prev = Some(token);
                        let (i, value) = match commands.pop().unwrap().0 {
                            Command::Let(i, value) => (i, value),
                            _ => unreachable!("There has to be Let command"),
                        };
                        let target = Box::new(self.parse(tokens));
                        let let_ = ExprType::Let(i, Box::new(value), target);
                        chain_apps(&mut expr, Expr::new(let_, from, to));
                    },
                },
            }
        }
        expr.unwrap_or_else(||
            Expr::new(ExprType::Error(ParseError::MissingTokens), to, to))
    }
}

#[test]
fn identity_abstraction() {
    use self::ExprType::*;
    assert_eq!(
        ::parse("λx.x"),
        Expr::new(Abs(
            "x".into(),
            None,
            Box::new(Expr::new(Var(
                "x".into()
            ), (0, 3), (0, 4)))
        ), (0, 0), (0, 4))
    );
    assert_eq!(
        ::parse("(λx.x)"),
        Expr::new(Abs(
            "x".into(),
            None,
            Box::new(Expr::new(Var(
                "x".into()
            ), (0, 4), (0, 5)))
        ), (0, 0), (0, 6))
    );
    assert_eq!(
        ::parse("λx.(x)"),
        Expr::new(Abs(
            "x".into(),
            None,
            Box::new(Expr::new(Var(
                "x".into()
            ), (0, 3), (0, 6)))
        ), (0, 0), (0, 6))
    );
}

#[test]
fn multiple_param_abstraction() {
    use self::ExprType::*;
    assert_eq!(
        ::parse("λx y.x"),
        Expr::new(Abs(
            "x".into(),
            None,
            Box::new(Expr::new(Abs(
                "y".into(),
                None,
                Box::new(Expr::new(Var(
                    "x".into()
                ), (0, 5), (0, 6)))
            ), (0, 3), (0, 6)))
        ), (0, 0), (0, 6))
    );
    assert_eq!(
        ::parse("λx.λy.x"),
        Expr::new(Abs(
            "x".into(),
            None,
            Box::new(Expr::new(Abs(
                "y".into(),
                None,
                Box::new(Expr::new(Var(
                    "x".into()
                ), (0, 6), (0, 7)))
            ), (0, 3), (0, 7)))
        ), (0, 0), (0, 7))
    );
}

#[test]
fn type_annotations() {
    use self::ExprType::*;
    assert_eq!(
        ::parse("λx:ϱ y z:σ.x"),
        Expr::new(Abs(
            "x".into(),
            Some("ϱ".into()),
            Box::new(Expr::new(Abs(
                "y".into(),
                None,
                Box::new(Expr::new(Abs(
                    "z".into(),
                    Some("σ".into()),
                    Box::new(Expr::new(Var(
                        "x".into()
                    ), (0, 11), (0, 12)))
                ), (0, 7), (0, 12)))
            ), (0, 5), (0, 12)))
        ), (0, 0), (0, 12))
    );
}

#[test]
fn application() {
    use self::ExprType::*;
    assert_eq!(
        ::parse("a b c"),
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
    assert_eq!(
        ::parse("(d e) f"),
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
    assert_eq!(
        ::parse("g (h) j"),
        Expr::new(App(
            Box::new(Expr::new(App(
                Box::new(Expr::new(Var(
                    "g".into(),
                ), (0, 0), (0, 1))),
                Box::new(Expr::new(Var(
                    "h".into(),
                ), (0, 2), (0, 5)))
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
    assert_eq!(
        ::parse("let x = y in z"),
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


#[test]
fn end_to_end() {
    let code = "let x = λx:σ y z:ϱ.x in x (λx.x x) x (λx.x (x x)) x";
    assert_eq!(
        format!("{}", ::parse(code).expr),
        code
    );
}