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

impl Expression {
    pub fn is_free(&self, i: &Ident) -> bool {
        use self::Expression::*;
        match self {
            Var(_) => true,
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

pub fn interpret(expr: Expr) -> Expression {
    fn from_expr(expr: Expr) -> Expression {
        use parser::ExprType::*;
        use self::Expression as E;
        let b = Box::new;
        match expr.expr {
            Error(e) => panic!("{:?}", e),
            Var(i) => E::Var(i),
            App(e1, e2) => E::App(b(from_expr(*e1)), b(from_expr(*e2))),
            Abs(i, e) => E::Abs(i, b(from_expr(*e))),
            Let(i, val, e) => E::Let(i, b(from_expr(*val)), b(from_expr(*e))),
        }
    }
    let mut ident_gen = IdentGen::new();
    let mut expr = from_expr(expr);
    loop {
        fn normal_reduction(ident_gen: &mut IdentGen, expr: Expression) -> Option<Expression> {
            use self::Expression::*;
            let b = Box::new;
            match expr {
                Var(_) => None,
                App(e1, e2) => {
                    if let &Abs(..) = &*e1 {
                        Some(beta_reduce(ident_gen, App(e1, e2)))
                    } else {
                        normal_reduction(ident_gen, *e1.clone())
                            .map(|e1| App(b(e1), e2.clone()))
                            .or_else(||
                                normal_reduction(ident_gen, *e2)
                                    .map(|e2| App(e1, b(e2)))
                            )
                    }
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

pub fn rename(expr: Expression, from: Ident, to: Ident) -> Expression {
    use self::Expression::*;
    let b = Box::new;
    match expr {
        Var(i) => if i == from {
            Var(to)
        } else {
            Var(i)
        },
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
    use self::Expression::*;
    let b = Box::new;
    assert_eq!(
        Abs("y".into(), b(Var("z".into()))),
        rename(Abs("y".into(), b(Var("x".into()))), "x".into(), "z".into())
    );
}

#[test]
fn rename_binding() {
    use self::Expression::*;
    let b = Box::new;
    assert_eq!(
        Abs("x".into(), b(Var("x".into()))),
        rename(Abs("x".into(), b(Var("x".into()))), "x".into(), "y".into())
    );
}

#[test]
fn rename_variable() {
    use self::Expression::*;
    assert_eq!(
        Var("y".into()),
        rename(Var("x".into()), "x".into(), "y".into())
    );
}

pub fn alpha_convert(expr: Expression, to: Ident) -> Expression {
    use self::Expression::*;
    match expr {
        Abs(i, expr) => {
            if expr.is_free(&to) || expr.is_binding(&to) {
                Abs(i, expr)
            } else {
                Abs(to.clone(), Box::new(rename(*expr, i, to)))
            }
        },
        e => e,
    }
}

#[test]
fn alpha_convert_variable() {
    use self::Expression::*;
    assert_eq!(
        Var("x".into()),
        alpha_convert(Var("x".into()), "y".into())
    );
}

#[test]
fn alpha_convert_free() {
    use self::Expression::*;
    let b = Box::new;
    assert_eq!(
        Abs("z".into(), b(Var("x".into()))),
        alpha_convert(Abs("z".into(), b(Var("x".into()))), "x".into())
    );
}

#[test]
fn alpha_convert_binding() {
    use self::Expression::*;
    let b = Box::new;
    assert_eq!(
        Abs("x".into(), b(Abs("y".into(), b(Var("x".into()))))),
        alpha_convert(Abs("x".into(), b(Abs("y".into(), b(Var("x".into()))))), "y".into())
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
        abs @ Abs(..) => {
            let fresh = ident_gen.next();
            substitute(
                ident_gen,
                alpha_convert(abs, fresh),
                ident,
                value
            )
        }
        Let(i, val, e) => unimplemented!("What are semantics here?"),
    }
}

pub fn beta_reduce(ident_gen: &mut IdentGen, expr: Expression) -> Expression {
    use self::Expression::*;
    match expr {
        App(e1, e2) => {
            // TODO: This is needed because bug in rust
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
    use self::Expression::*;
    let b = Box::new;
    let mut ident_gen = IdentGen::new();
    assert_eq!(
        Var("y".into()),
        beta_reduce(
            &mut ident_gen, 
            App(
                b(Abs(
                    "x".into(),
                    b(Var("x".into())),
                )),
                b(Var("y".into()))
            )
        )
    );
}

#[test]
fn reduce_omega() {
    use self::Expression::*;
    let b = Box::new;
    let mut ident_gen = IdentGen::new();
    let omega = App(
        b(Abs(
            "x".into(),
            b(App(
                b(Var("x".into())),
                b(Var("x".into()))
            ))
        )),
        b(Abs(
            "x".into(),
            b(App(
                b(Var("x".into())),
                b(Var("x".into()))
            ))
        ))
    );
    assert_eq!(
        omega.clone(),
        beta_reduce(&mut ident_gen, omega)
    );
}
