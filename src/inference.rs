
use std::collections::HashSet;
use std::collections::HashMap;
use std::fmt;

use ena::unify::{UnifyKey, UnifyValue};
use ena::unify::InPlaceUnificationTable as UnificationTable;

use {Ident, Type};

#[derive(Debug, Clone)]
pub struct Expr {
    expr: Expression,
    id: Ident,
}

impl Expr {
    pub fn new(expr: Expression, id: Ident) -> Self {
        Expr {
            expr,
            id
        }
    }
}

impl From<::parser::Expr> for Expr {
    fn from(e: ::parser::Expr) -> Self {
        use parser::ExprType::*;
        use self::Expression as E;
        fn rec(e: ::parser::Expr, n: &mut u64, vars: &mut HashMap<Ident, Ident>) -> Expr {
            let b = Box::new;
            match e.expr {
                Error(e) => panic!("{:?}", e),
                Var(i) => {
                    let i = vars.entry(i).or_insert_with(|| {
                        let ident = format!("@{}", n);
                        *n += 1;
                        ident
                    });
                    Expr::new(E::Var(i.clone()), i.clone())
                },
                App(e1, e2) => {
                    let e1 = rec(*e1, n, vars);
                    *n += 1;
                    let e2 = rec(*e2, n, vars);
                    *n += 1;
                    Expr::new(E::App(b(e1), b(e2)), format!("@{}", n))
                },
                Abs(i, ty, e) => {
                    let e = rec(*e, n, vars);
                    *n += 1;
                    Expr::new(E::Abs(vars.entry(i).or_insert_with(|| {
                        let ident = format!("@{}", n);
                        *n += 1;
                        ident
                    }).clone(), ty.map(|ty| vars.entry(ty).or_insert_with(|| {
                        let ident = format!("@{}", n);
                        *n += 1;
                        ident
                    }).clone()), b(e)), format!("@{}", n))
                },
                Let(i, val, e) => {
                    let val = rec(*val, n, vars);
                    *n += 1;
                    let e = rec(*e, n, vars);
                    *n += 1;
                    Expr::new(E::Let(vars.entry(i).or_insert_with(|| {
                        let ident = format!("@{}", n);
                        *n += 1;
                        ident
                    }).clone(), b(val), b(e)), format!("@{}", n))
                },
            }
        }
        rec(e, &mut 0, &mut HashMap::new())
    }
}

impl PartialEq for Expr {
    fn eq(&self, lhs: &Self) -> bool {
        self.id == lhs.id
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Var(Ident),
    App(Box<Expr>, Box<Expr>),
    Abs(Ident, Option<Type>, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Mono {
    Var(Ident),
    App(TyFun),
}

impl Mono {
    fn free(&self) -> HashSet<Ident> {
        use self::Mono::*;
        match self {
            Var(i) => collect![i.clone()],
            App(a) => a.free(),
        }
    }

    fn contains(&self, ident: &Ident) -> bool {
        use self::Mono::*;
        match self {
            Var(i) => i == ident,
            App(TyFun { params, ..}) => params.iter().any(|p| p.contains(ident))
        }
    }
}

impl fmt::Display for Mono {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Mono::*;
        match self {
            Var(i) => fmt::Display::fmt(i, fmt),
            App(f) => write!(fmt, "({})", f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyFun {
    name: Ident,
    params: Vec<Mono>,
}

impl TyFun {
    fn new_fn(param: Mono, result: Mono) -> TyFun {
        TyFun {
            name: "->".into(),
            params: vec![param, result],
        }
    }

    fn integer() -> TyFun {
        TyFun {
            name: "I32".into(),
            params: vec![],
        }
    }

    fn free(&self) -> HashSet<Ident> {
        self.params.iter()
            .flat_map(Mono::free)
            .collect()
    }
}

impl fmt::Display for TyFun {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.name == "->" {
            write!(fmt, "{}→{}", self.params[0], self.params[1])
        } else {
            write!(fmt, "{} (", self.name)?;
            let mut fst = true;
            for p in &self.params {
                if fst {
                    fst = false;
                } else {
                    fmt.write_str(" ")?;
                }
                write!(fmt, "{}", p)?;
            }
            fmt.write_str(")")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Poly {
    Mono(Mono),
    Quan(Ident, Box<Poly>),
}


impl fmt::Display for Poly {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Poly::*;
        match self {
            Mono(m) => fmt::Display::fmt(m, fmt),
            Quan(i, p) => write!(fmt, "∀{}.{}", i, p),
        }
    }
}

impl Poly {
    fn free(&self) -> HashSet<Ident> {
        use self::Poly::*;
        match self {
            Mono(m) => m.free(),
            Quan(i, p) => {
                let mut free = p.free();
                free.remove(i);
                free
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    types: HashMap<Ident, Poly>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            types: HashMap::new(),
        }
    }

    pub fn get(&self, i: &Ident) -> Option<&Poly> {
        self.types.get(i)
    }
    
    pub fn insert(&mut self, i: Ident, p: Poly) -> Option<Poly> {
        self.types.insert(i, p)
    }

    pub fn extend(&mut self, rhs: Context) {
        self.types.extend(rhs.types.into_iter());
    }

    pub fn free(&self) -> HashSet<Ident> {
        self.types.values()
            .flat_map(Poly::free)
            .collect()
    }
}

impl fmt::Display for Context {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("Context {\n")?;
        for (k, v) in &self.types {
            write!(fmt, "  {} => {}\n", k, v)?;
        }
        fmt.write_str("}")
    }
}

pub fn inst(ty: &Poly, m: &mut i32, table: &mut UnificationTable<InferVar>) -> Mono {
    fn gather_polys<'a>(ty: &'a Poly, replacements: &mut HashMap<Ident, Ident>, m: &mut i32, table: &mut UnificationTable<InferVar>) -> &'a Mono {
        use self::Poly::*;
        match ty {
            Quan(i, e) => {
                let n = *m;
                *m += 1;
                replacements.insert(i.clone(), format!("#{}", table.new_key(InferVal::Unbound(n)).0));
                gather_polys(e, replacements, m, table)
            },
            Mono(m) => m,
        }
    }
    let mut replacements = HashMap::new();
    let ty = gather_polys(ty, &mut replacements, m, table);
    fn replace(ty: &Mono, replacements: &HashMap<Ident, Ident>) -> Mono {
        use self::Mono::*;
        match ty {
            App(f) => App(TyFun {
                name: f.name.clone(),
                params: f.params.iter().map(|i| replace(i, replacements)).collect()
            }),
            Var(i) => Var(replacements.get(i).unwrap_or(i).clone())
        }
    }
    replace(ty, &replacements)
}

pub fn new_var(m: &mut i32, table: &mut UnificationTable<InferVar>) -> Type {
    let n = *m;
    *m += 1;
    format!("#{}", table.new_key(InferVal::Unbound(n)).0)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferVar(u32);

impl fmt::Debug for InferVar {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("InferVar(")?;
        write!(fmt, "{}", self.0)?;
        fmt.write_str(")")
    }
}

impl UnifyKey for InferVar {
    type Value = InferVal;
    fn index(&self) -> u32 {
        self.0
    }
    fn from_index(u: u32) -> Self {
        InferVar(u)
    }
    fn tag() -> &'static str {
        "TypeKey"
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InferVal {
    Unbound(i32),
    Bound(Mono) // TODO: What should be here?
}

impl UnifyValue for InferVal {
    type Error = ();
    fn unify_values<'a>(v1: &'a Self, v2: &'a Self) -> ::std::result::Result<Self, Self::Error> {
        use self::InferVal::*;
        Ok(match (v1, v2) {
            (&Unbound(ref i1), &Unbound(ref i2)) => Unbound(*i1.min(i2)),
            (b @ &Bound(_), &Unbound(_)) | (&Unbound(_), b @ &Bound(_)) => b.clone(),
            (&Bound(_), &Bound(_)) => Err(())?,
        })
    }
}

pub fn unify(ta: &Mono, tb: &Mono, table: &mut UnificationTable<InferVar>) {
    use self::Mono::*;
    //In other words, you first check if the LHS or RHS has been unified with a concrete type.
    //If both have not, then you unify the two variables. If either has, then you "normalize" and repeat:

    println!("{:?} {:?}", ta, tb);

    if let Var(ta) = ta {
        if let InferVal::Bound(i) = table.probe_value(InferVar(ta[1..].parse().unwrap())) {
            unify(&i, tb, table);
            return;
        }
    }
    if let Var(tb) = tb {
        if let InferVal::Bound(i) = table.probe_value(InferVar(tb[1..].parse().unwrap())) {
            unify(ta, &i, table);
            return;
        }
    }
    match (ta, tb) {
        (App(TyFun { name: n1, params: p1 }), App(TyFun { name: n2, params: p2 })) if n1 == n2 && p1.len() == p2.len() => {
            for (p1, p2) in p1.iter().zip(p2.iter()) {
                unify(p1, p2, table);
            }
        },
        (Var(a), Var(b)) => {
            table.unify_var_var(InferVar(a[1..].parse().unwrap()), InferVar(b[1..].parse().unwrap())).unwrap();
        }
        (Var(a), b) | (b, Var(a)) => {
            if b.contains(a) {
                panic!("`{}` occurs in `{}`", a, b);
            }
            table.unify_var_value(InferVar(a[1..].parse().unwrap()), InferVal::Bound(b.clone())).unwrap();
        },
        (a, b) => panic!("{:?}, {:?}", a, b),
    }
}

pub fn quantify(m: &Mono, ctx: &Context) -> Poly {
    let mut m = Poly::Mono(m.clone());
    for var in m.free().difference(&ctx.free()) {
        m = Poly::Quan(var.clone(), Box::new(m));
    }
    m
}

pub fn infer(expr: &Expr, mut ctx: Context, m: &mut i32, table: &mut UnificationTable<InferVar>) -> Context {
    use self::Expression::*;
    match &expr.expr {
        Var(i) => {
            let ty = inst(ctx.get(&expr.id).expect("Unbound variable"), m, table);
            ctx.insert(expr.id.clone(), Poly::Mono(ty));
        },
        App(lhs, rhs) => {
            let lhs_ctx = infer(lhs, ctx.clone(), m, table);
            // TODO: Because of type annotation this fails. Switch to numbering
            let lhs_ty = if let Poly::Mono(m) = lhs_ctx.get(&lhs.id).unwrap().clone() {
                m
            } else {
                panic!("TODO");
            };
            let rhs_ctx = infer(rhs, ctx.clone(), m, table);
            let rhs_ty = if let Poly::Mono(m) = rhs_ctx.get(&rhs.id).unwrap().clone() {
                m
            } else {
                panic!("TODO");
            };
            ctx.extend(lhs_ctx);
            ctx.extend(rhs_ctx);
            let ty = Mono::Var(new_var(m, table));
            unify(&lhs_ty, &Mono::App(TyFun::new_fn(rhs_ty.clone(), ty.clone())), table);
            ctx.insert(expr.id.clone(), Poly::Mono(ty));
        },
        Abs(i, ty1, body) => {
            let ty = ty1.clone().unwrap_or_else(|| new_var(m, table));
            let mut new_ctx = ctx.clone();
            new_ctx.insert(i.clone(), Poly::Mono(Mono::Var(ty.clone())));
            let ret_ctx = infer(body, new_ctx, m, table);
            let ret_ty = if let Poly::Mono(m) = ret_ctx.get(&body.id).unwrap().clone() {
                m
            } else {
                panic!("TODO");
            };
            ctx.extend(ret_ctx);
            ctx.insert(expr.id.clone(), Poly::Mono(Mono::App(TyFun::new_fn(Mono::Var(ty), ret_ty))));
        },
        Let(var, val, body) => {
            let val_ctx = infer(val, ctx.clone(), m, table);
            let val_ty = if let Poly::Mono(m) = val_ctx.get(&val.id).unwrap().clone() {
                m
            } else {
                panic!("TODO");
            };
            ctx.extend(val_ctx);
            let mut new_ctx = ctx.clone();
            new_ctx.insert(var.clone(), quantify(&val_ty, &ctx));
            let body_ty = infer(body, new_ctx, m, table).get(&body.id).unwrap().clone();
            ctx.insert(body.id.clone(), body_ty.clone());
            ctx.insert(expr.id.clone(), body_ty);
        },
    }
    ctx
}

#[test]
fn infer_identity_abstraction() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("λx.x".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[test]
fn infer_annotated_identity_abstraction() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("λx:σ.x".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[test]
fn infer_annotated() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("λz.(λx:σ y.y x) z".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[test]
fn infer_let() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("let x = λx.x in λy.x".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[test]
fn infer_app() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("(λx.x)λy.y".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[test]
fn infer_free() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("λx.y".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[test]
fn infer_variable_is_integer() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    panic!("{}", infer(&parser.parse(&mut lexer("(λx.x)".chars())).into(), Context::new(), &mut 0, &mut table));
}

#[should_panic]
#[test]
fn infer_self_application() {
    use lexer::lexer;
    use parser::Parser;
    let mut table = UnificationTable::new();
    let mut parser = Parser::new();
    infer(&parser.parse(&mut lexer("λx.x x".chars())).into(), Context::new(), &mut 0, &mut table);
}

// pub struct InferenceTable {
//     unify: UnificationTable<InferVar>,
//     vars: Vec<InferVar>,
//     // max_universe: UniverseIndex,
// }

// impl InferenceTable {
//     fn normalize_shallow(&mut self, leaf: &Poly, binders: usize) -> Option<Poly> {
//         leaf.var().and_then(|depth| {
//             if depth < binders {
//                 None // bound variable, not an inference var
//             } else {
//                 let var = InferVar::from_depth(depth - binders);
//                 match self.unify.probe_value(var) {
//                     InferVal::Unbound(_) => None,
//                     InferVal::Bound(ref val) => {
//                         let ty = val.as_ref().ty().unwrap();
//                         Some(ty.up_shift(binders))
//                     }
//                 }
//             }
//         })
//     }
// }

// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
// pub enum Mono {
//     Var(Ident),
//     App(TyFun),
// }

// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
// pub struct TyFun {
//     name: Ident,
//     params: Vec<InferVar>,
// }

// impl TyFun {
//     fn new_fn(param: InferVar, result: InferVar) -> TyFun {
//         TyFun {
//             name: "->".into(),
//             params: vec![param, result],
//         }
//     }

//     fn integer() -> TyFun {
//         TyFun {
//             name: "I32".into(),
//             params: vec![],
//         }
//     }
// }

// impl Mono {
//     pub fn free(&self, ut: &mut UnificationTable<InferVar>) -> HashSet<Ident> {
//         use self::Mono::*;
//         match *self {
//             Var(ref i) => collect![i.clone()],
//             App(TyFun{ref params, ..}) => params.iter().flat_map(|v| ut.probe_value(*v).free(ut)).collect(),
//         }
//     }
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum Poly {
//     Mono(Mono, InferVar),
//     Quan(Ident, Box<Poly>),
// }

// impl Poly {
//     pub fn free(&self, ut: &mut UnificationTable<InferVar>) -> HashSet<Ident> {
//         use self::Poly::*;
//         match *self {
//             Mono(ref m, _) => m.free(ut),
//             Quan(ref i, ref p) => {
//                 let mut free = p.free(ut);
//                 free.remove(i);
//                 free
//             },
//         }
//     }
// }

// pub struct Ctx {
//     types: HashMap<Ident, Poly>,
// }

// impl Ctx {
//     pub fn new() -> Self {
//         Ctx {
//             types: HashMap::new(),
//         }
//     }

//     pub fn get(&self, i: Ident) -> Option<&Poly> {
//         self.types.get(&i)
//     }

//     pub fn free(&self, ut: &mut UnificationTable<InferVar>) -> HashSet<Ident> {
//         self.types.iter().flat_map(|t| t.1.free(ut)).collect()
//     }

//     pub fn insert(&mut self, i: Ident, p: Poly) -> Option<Poly> {
//         self.types.insert(i, p)
//     }
// }

// pub struct VarCreator(usize);

// impl VarCreator {
//     pub fn new() -> Self {
//         VarCreator(0)
//     }

//     pub fn create(&mut self) -> Ident {
//         self.0 += 1;
//         format!("i{}", self.0)
//     }
// }