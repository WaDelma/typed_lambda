use std::fmt;

use parser::Expr;
use Ident;

pub struct IdentGen(u64);

impl IdentGen {
    pub fn new() -> Self {
        IdentGen(0)
    }
    pub fn next(&mut self) -> Ident {
        let ident = format!("#{}", self.0);
        self.0 += 1;
        ident
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Var(Ident),
    App(Box<Expression>, Box<Expression>),
    Abs(Ident, Box<Expression>),
    Let(Ident, Box<Expression>, Box<Expression>),
}

impl From<Expr> for Expression {
    fn from(e: Expr) -> Self {
        use parser::ExprType::*;
        use self::Expression as E;
        let b = Box::new;
        match e.expr {
            Error(e) => panic!("{:?}", e),
            Var(i) => E::Var(i),
            App(e1, e2) => E::App(b((*e1).into()), b((*e2).into())),
            Abs(i, _, e) => E::Abs(i, b((*e).into())),
            Let(i, val, e) => E::Let(i, b((*val).into()), b((*e).into())),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expression::*;
        match self {
            Var(ident) => fmt.write_str(ident),
            App(lhs, rhs) => {
                fn apps(lhs: &Expression, fmt: &mut fmt::Formatter) -> fmt::Result {
                    match lhs {
                        App(lhs, rhs) => {
                            apps(&lhs, fmt)?;
                            match **rhs {
                                Var(_) => write!(fmt, " {}", rhs),
                                _ => write!(fmt, " ({})", rhs),
                            }
                        }
                        Var(_) => write!(fmt, "{}", lhs),
                        _ => write!(fmt, "({})", lhs)
                    }
                }
                apps(&lhs, fmt)?;
                match **rhs {
                    App(..) => write!(fmt, " ({})", rhs),
                    _ => write!(fmt, " {}", rhs),
                }
            },
            Abs(ident, rhs) => {
                write!(fmt, "λ{}", ident)?;
                fn abss(rhs: &Expression, fmt: &mut fmt::Formatter) -> fmt::Result {
                    match rhs {
                        Abs(ident, rhs) => {
                            write!(fmt, " {}", ident)?;
                            abss(&rhs, fmt)
                        },
                        _ => write!(fmt, ".{}", rhs)
                    }
                }
                abss(&rhs, fmt)
            },
            Let(ident, value, target) => write!(fmt, "let {} = {} in {}", ident, value, target),
        }
    }
}

impl Expression {
    pub fn is_alpha_equivalent(&self, lhs: &Self) -> bool {
        use std::collections::HashMap;
        // TODO: Refactor this out. Maybe use de bruijn numbering everywhere.
        fn de_bruijnify(s: &Expression, m: isize, mut depths: HashMap<Ident, isize>) -> String {
            use self::Expression::*;
            match s {
                Var(i) => depths.get(i).map(|d| (m - d).to_string()).unwrap_or_else(|| i.clone()),
                App(e1, e2) => format!("({} {})", de_bruijnify(e1, m, depths.clone()), de_bruijnify(e2, m, depths)),
                Abs(i, e) => {
                    depths.insert(i.clone(), m);
                    format!("(λ{})", de_bruijnify(&e, m + 1, depths))
                }
                Let(i, val, e) => {
                    depths.insert(i.clone(), m);
                    format!("(@{}→{})", de_bruijnify(&val, m + 1, depths.clone()), de_bruijnify(&e, m + 1, depths))
                }
            }
        }
        de_bruijnify(self, 0, HashMap::new()) == de_bruijnify(lhs, 0, HashMap::new())
    }

    pub fn is_free(&self, i: &Ident) -> bool {
        use self::Expression::*;
        match self {
            Var(j) => j == i,
            App(e1, e2) => e1.is_free(i) || e2.is_free(i),
            Abs(j, e) => j != i && e.is_free(i),
            Let(j, val, e) => j != i && val.is_free(i) && e.is_free(i),
        }
    }

    pub fn is_binding(&self, i: &Ident) -> bool {
        use self::Expression::*;
        match self {
            Var(_) => false,
            App(e1, e2) => e1.is_binding(i) || e2.is_binding(i),
            Abs(j, e) => j == i || e.is_binding(i),
            Let(j, val, e) => j == i || val.is_binding(i) && e.is_binding(i),
        }
    }
}

#[test]
pub fn are_exprs_alpha_equivalent() {
    use self::Expression as E;
    assert!(E::from(::parse("x")).is_alpha_equivalent(&E::from(::parse("x"))));
    assert!(!E::from(::parse("x")).is_alpha_equivalent(&E::from(::parse("y"))));
    assert!(E::from(::parse("λx.x")).is_alpha_equivalent(&E::from(::parse("λx.x"))));
    assert!(E::from(::parse("λx.x")).is_alpha_equivalent(&E::from(::parse("λy.y"))));
    assert!(E::from(::parse("λx y.x y")).is_alpha_equivalent(&E::from(::parse("λy x.y x"))));
    assert!(!E::from(::parse("λx x.x y")).is_alpha_equivalent(&E::from(::parse("λx y.x y"))));
    assert!(!E::from(::parse("λx y.x x")).is_alpha_equivalent(&E::from(::parse("λx y.x y"))));
    assert!(E::from(::parse("λx.x x x")).is_alpha_equivalent(&E::from(::parse("λy.y y y"))));
    assert!(!E::from(::parse("λx.x x x")).is_alpha_equivalent(&E::from(::parse("λy.y (y y)"))));
}

pub fn rename(expr: Expression, from: Ident, to: Ident) -> Expression {
    use self::Expression::*;
    let b = Box::new;
    match expr {
        Var(i) => Var(if i == from {
            to
        } else {
            i
        }),
        App(l, r) => App(
            b(rename(*l, from.clone(), to.clone())),
            b(rename(*r, from, to))
        ),
        Abs(i, e) => if i == from {
            Abs(i, e)
        } else {
            Abs(i, b(rename(*e, from, to)))
        },
        Let(i, val, e) => if i == from {
            Let(i, b(rename(*val, from, to)), e)
        } else {
            Let(
                i,
                b(rename(*val, from.clone(), to.clone())),
                b(rename(*e, from, to))
            )
        },
    }
}

#[test]
fn rename_abstraction() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("λy.z")),
        rename(E::from(::parse("λy.x")), "x".into(), "z".into())
    );
}

#[test]
fn rename_binding() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("λx.x")),
        rename(E::from(::parse("λx.x")), "x".into(), "y".into())
    );
}

#[test]
fn rename_variable() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("y")),
        rename(E::from(::parse("x")), "x".into(), "y".into())
    );
}

pub fn alpha_convert(expr: Expression, to: Ident) -> Expression {
    use self::Expression::*;
    match expr {
        Abs(i, expr) => if expr.is_free(&to) || expr.is_binding(&to) {
            Abs(i, expr)
        } else {
            Abs(to.clone(), Box::new(rename(*expr, i, to)))
        },
        e => e,
    }
}

#[test]
fn alpha_convert_variable() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("x")),
        alpha_convert(E::from(::parse("x")), "y".into())
    );
}

#[test]
fn alpha_convert_free() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("λz.x")),
        alpha_convert(E::from(::parse("λz.x")), "x".into())
    );
}

#[test]
fn alpha_convert_binding() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("λx y.z")),
        alpha_convert(E::from(::parse("λx y.z")), "y".into())
    );
}

pub fn substitute(ident_gen: &mut IdentGen, expr: Expression, ident: Ident, value: Expression) -> Expression {
    use self::Expression::*;
    let b = Box::new;
    match expr {
        Var(i) => if i == ident {
            value
        } else {
            Var(i)
        },
        App(e1, e2) => App(
            b(substitute(ident_gen, *e1, ident.clone(), value.clone())),
            b(substitute(ident_gen, *e2, ident, value)),
        ),
        abs @ Abs(..) => if let Abs(i, e) = alpha_convert(abs, ident_gen.next()) {
            Abs(i, b(substitute(ident_gen, *e, ident, value)))
        } else {
            unreachable!("Alpha convert on abstraction shouldn't change expressions type.");
        },
        Let(i, val, e) => unimplemented!("What are semantics here?"),
    }
}

#[test]
fn substitute_variable() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("λy.y")),
        substitute(
            &mut IdentGen::new(),
            E::from(::parse("x")), "x".into(), E::from(::parse("λy.y"))
        )
    );
}

#[test]
fn substitute_in_abstraction() {
    use self::Expression as E;
    assert!(
        E::from(::parse("λy.x")).is_alpha_equivalent(
            &substitute(
                &mut IdentGen::new(),
                E::from(::parse("λx.y")), "y".into(), E::from(::parse("x"))
            )
        )
    );
}

#[test]
fn substitute_with_abstraction() {
    use self::Expression as E;
    assert!(
        E::from(::parse("λx.(λx.y) x z")).is_alpha_equivalent(
            &substitute(
                &mut IdentGen::new(),
                E::from(::parse("λy.x y z")),
                "x".into(),
                E::from(::parse("λx.y"))
            )
        )
    );
}

pub fn beta_reduce(ident_gen: &mut IdentGen, expr: Expression) -> Expression {
    use self::Expression::*;
    match expr {
        App(e1, e2) => {
            // TODO: This is needed because bug in rustc
            let e1 = *e1;
            match e1 {
                Abs(i, e) => {
                    substitute(ident_gen, *e, i, *e2)
                },
                e => e,
            }
        },
        e => e,
    }
}

#[test]
fn reduce_identity() {
    use self::Expression as E;
    assert_eq!(
        E::from(::parse("y")),
        beta_reduce(
            &mut IdentGen::new(),
            E::from(::parse("(λx.x)y"))
        )
    );
}

#[test]
fn reduce_omega() {
    use self::Expression as E;
    let omega = E::from(::parse("(λx.x x)λx.x x"));
    assert_eq!(
        omega.clone(),
        beta_reduce(&mut IdentGen::new(), omega)
    );
}


#[test]
fn reduce_application() {
    use self::Expression as E;
    assert!(
        E::from(::parse("λy.(λx y. x) y")).is_alpha_equivalent(
            &beta_reduce(
                &mut IdentGen::new(),
                E::from(::parse("(λx y.x y)λx y.x"))
            )
        )
    );
    assert!(
        E::from(::parse("λy.(λx y.x) y λx y.y")).is_alpha_equivalent(
            &beta_reduce(
                &mut IdentGen::new(),
                E::from(::parse("(λx y.x y λx y.y) λx y.x"))
            )
        )
    );
}

pub fn interpret(mut expr: Expression) -> Expression {
    let mut ident_gen = IdentGen::new();
    loop {
        fn normal_reduction(ident_gen: &mut IdentGen, expr: Expression) -> Option<Expression> {
            use self::Expression::*;
            let b = Box::new;
            match expr {
                Var(_) => None,
                App(e1, e2) => if let Abs(..) = *e1 {
                    Some(beta_reduce(ident_gen, App(e1, e2)))
                } else {
                    normal_reduction(ident_gen, *e1.clone())
                        .map(|e1| App(b(e1), e2.clone()))
                        .or_else(||
                            normal_reduction(ident_gen, *e2)
                                .map(|e2| App(e1, b(e2)))
                        )
                },
                Abs(_, e) => normal_reduction(ident_gen, *e),
                Let(i, val, e) => normal_reduction(ident_gen, *val.clone())
                    .map(|val| Let(i.clone(), b(val), e.clone()))
                    .or_else(||
                        normal_reduction(ident_gen, *e)
                            .map(|e| Let(i, val, b(e)))
                    )
            }
        }
        if let Some(e) = normal_reduction(&mut ident_gen, expr.clone()) {
            expr = e;
        } else {
            break;
        }
    }
    expr
}

#[test]
fn interpret_and() {
    use self::Expression as E;
    let true_: E = ::parse("λx y.x").into();
    let false_: E = ::parse("λx y.y").into();
    let and: E = ::parse(&format!("λx y.x y {}", false_)).into();
    let and_00 = ::parse(&format!("({}) ({}) {}", and, false_, false_)).into();
    assert!(interpret(and_00).is_alpha_equivalent(&false_));
    let and_01 = ::parse(&format!("({}) ({}) {}", and, false_, false_)).into();
    assert!(interpret(and_01).is_alpha_equivalent(&false_));
    let and_10 = ::parse(&format!("({}) ({}) {}", and, false_, false_)).into();
    assert!(interpret(and_10).is_alpha_equivalent(&false_));
    let and_11 = ::parse(&format!("({}) ({}) {}", and, true_, true_)).into();
    assert!(interpret(and_11).is_alpha_equivalent(&true_));
}

#[test]
fn interpret_not() {
    use self::Expression as E;
    let true_: E = ::parse("λx y.x").into();
    let false_: E = ::parse("λx y.y").into();
    let not: E = ::parse(&format!("λx.x ({}) {}", false_, true_)).into();
    let not_0 = ::parse(&format!("({}) {}", not, true_)).into();
    assert!(interpret(not_0).is_alpha_equivalent(&false_));
    let not_1 = ::parse(&format!("({}) {}", not, false_)).into();
    assert!(interpret(not_1).is_alpha_equivalent(&true_));
}