#[macro_use]
extern crate collect_mac;
extern crate ena;

use ena::unify::{UnifyKey, UnifyValue};
use ena::unify::InPlaceUnificationTable as UnificationTable;

use std::collections::{HashMap, HashSet};
use std::fmt;

use self::TypecheckError::*;

type Ident = String;

#[derive(Debug, PartialEq)]
pub enum TypecheckError {
    LetDepth,
    UnboundVar(Ident),
}

pub type Result<T> = std::result::Result<T, TypecheckError>;

pub enum Expr {
    Var(Ident),
    App(Box<Expr>, Box<Expr>),
    Abs(Ident, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Mono {
    Var(Ident),
    App(TyFun),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TyFun {
    name: Ident,
    params: Vec<Key>,
}

impl TyFun {
    fn new_fn(param: Key, result: Key) -> TyFun {
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
}

impl Mono {
    pub fn free(&self, ut: &mut UnificationTable<Key>) -> HashSet<Ident> {
        use self::Mono::*;
        match *self {
            Var(ref i) => collect![i.clone()],
            App(TyFun{ref params, ..}) => params.iter().flat_map(|v| ut.probe_value(*v).free(ut)).collect(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Poly {
    Mono(Mono, Key),
    Quan(Ident, Box<Poly>),
}

impl Poly {
    pub fn free(&self, ut: &mut UnificationTable<Key>) -> HashSet<Ident> {
        use self::Poly::*;
        match *self {
            Mono(ref m, _) => m.free(ut),
            Quan(ref i, ref p) => {
                let mut free = p.free(ut);
                free.remove(i);
                free
            },
        }
    }
}

pub struct Ctx {
    types: HashMap<Ident, Poly>,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            types: HashMap::new(),
        }
    }

    pub fn get(&self, i: Ident) -> Option<&Poly> {
        self.types.get(&i)
    }

    pub fn free(&self, ut: &mut UnificationTable<Key>) -> HashSet<Ident> {
        self.types.iter().flat_map(|t| t.1.free(ut)).collect()
    }

    pub fn insert(&mut self, i: Ident, p: Poly) -> Option<Poly> {
        self.types.insert(i, p)
    }
}

pub struct VarCreator(usize);

impl VarCreator {
    pub fn new() -> Self {
        VarCreator(0)
    }

    pub fn create(&mut self) -> Ident {
        self.0 += 1;
        format!("i{}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key(u32);

impl fmt::Debug for Key {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("Key(")?;
        write!(fmt, "{}", self.0)?;
        fmt.write_str(")")
    }
}

impl UnifyKey for Key {
    type Value = Mono;
    fn index(&self) -> u32 {
        self.0
    }
    fn from_index(u: u32) -> Self {
        Key(u)
    }
    fn tag() -> &'static str {
        "TypeKey"
    }
}

impl UnifyValue for Mono {
    type Error = ();
    fn unify_values(v1: &Self, v2: &Self) -> ::std::result::Result<Self, Self::Error> {
        use self::Mono::*;
        
        match (v1, v2) {
            (&App(TyFun { name: ref f1, params: ref v1 }), &App(TyFun { name: ref f2, params: ref v2 })) if f1 == f2 && v1.len() == v2.len() => {
                /*v1.iter().zip(v2.iter())
                    .map(|(a, b)| Mono::unify_values(a, b))
                    .collect::<::std::result::Result<Self, Self::Error>>()*/
                Err(())
            },
            (&Var(ref i), _) | (_, &Var(ref i)) => {
                Ok(Var(i.clone()))
            },
            _ => Err(()),
        }
    }
}

// TODO: new_key is called wrongly: There is multiple ununified same things there
pub fn check_expr(e: &Expr, ctx: &mut Ctx, creator: &mut VarCreator, ut: &mut UnificationTable<Key>, let_depth: usize) -> Result<Key> {
    use self::Expr::*;
    Ok(match *e {
        Var(ref ident) => {
            match str::parse::<i32>(ident) {
                Ok(_) => {
                    let res = ut.new_key(Mono::App(TyFun::integer()));
                    println!("Varval {:?}", res);
                    res
                }
                Err(_) => {
                    match ctx.types.get(ident).ok_or(UnboundVar(ident.clone()))?.clone() {
                        Poly::Mono(_, ref k) => {
                            *k
                        },
                        ref ty => {
                            let mono = inst(ty, creator, ut);
                            let res = ut.new_key(mono.clone());
                            ctx.insert(ident.clone(), Poly::Mono(mono, res));
                            println!("Varvar {} {:?}", ident, res);
                            res
                        }
                    }
                }
            }
        },
        App(ref fun, ref params) => {
            let newvar = ut.new_key(Mono::Var(creator.create()));
            println!("App1 {:?}", newvar);
            let f_type = check_expr(fun, ctx, creator, ut, let_depth)?;
            let p_type = check_expr(params, ctx, creator, ut, let_depth)?;
            let fun = ut.new_key(Mono::App(TyFun::new_fn(p_type, newvar)));
            println!("App2 {:?}", fun);
            unify(f_type, fun, ut);
            newvar
        },
        Abs(ref fun, ref body) => {
            println!("Abs1 {}", fun);
            let newvar = Mono::Var(creator.create());
            let param = ut.new_key(newvar.clone());
            ctx.insert(fun.clone(), Poly::Mono(newvar, param));
            println!("Abs2 {:?}", param);
            let fun = Mono::App(TyFun::new_fn(param, check_expr(body, ctx, creator, ut, let_depth)?));
            let res = ut.new_key(fun);
            println!("Abs3 {:?}", res);
            res
        },
        Let(ref ident, ref value, ref exprs) => {
            if let_depth == 0 {
                Err(LetDepth)?
            }
            let v_ty = check_expr(value, ctx, creator, ut, let_depth - 1)?;
            let x = quantify(&ut.probe_value(v_ty), v_ty, ut);
            ctx.insert(ident.clone(), x);
            check_expr(exprs, ctx, creator, ut, let_depth)?
        },
    })
}

fn quantify(m: &Mono, k: Key, ut: &mut UnificationTable<Key>) -> Poly {
    let mut result = Poly::Mono(m.clone(), k);
    for f in m.free(ut) {
        result = Poly::Quan(f, Box::new(result));
    }
    result
}

fn unify(a: Key, b: Key, ut: &mut UnificationTable<Key>) {
    use Mono::*;
    let va = ut.probe_value(a);
    let vb = ut.probe_value(b);
    match (va, vb) {
        (App(TyFun { name: ref f1, params: ref v1 }), App(TyFun { name: ref f2, params: ref v2 })) if f1 == f2 && v1.len() == v2.len() => {
            v1.iter().zip(v2.iter())
                .for_each(|(a, b)| unify(*a, *b, ut));
        },
        (Var(_), _) | (_, Var(_)) => {
            println!("unify {:?}, {:?}", a, b);
            ut.unify_var_var(a, b).unwrap();
        },
        _ => panic!("Cannot unify"),
    }
}

fn inst(p: &Poly, creator: &mut VarCreator, ut: &mut UnificationTable<Key>) -> Mono {
    fn rec_mono(m: &Mono, ctx: &mut HashMap<Ident, Ident>, ut: &mut UnificationTable<Key>) -> Mono {
        use self::Mono::*;
        match *m {
            Var(ref i) => Var(ctx.get(i).cloned().unwrap_or(i.clone())),
            App(TyFun { ref name, ref params }) => App(TyFun {
                name: name.clone(),
                params: params.iter().map(|m| {
                    let val = ut.probe_value(*m);
                    let mono = rec_mono(&val, ctx, ut);
                    ut.new_key(mono)
                }).collect()
            }),
        }
    }
    fn rec_poly(p: &Poly, ctx: &mut HashMap<Ident, Ident>, creator: &mut VarCreator, ut: &mut UnificationTable<Key>) -> Mono {
        use self::Poly::*;
        match *p {
            Mono(ref m, _) => rec_mono(m, ctx, ut),
            Quan(ref i, ref p) => {
                ctx.insert(i.clone(), creator.create());
                rec_poly(p, ctx, creator, ut)
            }
        }
    }
    match *p {
        Poly::Mono(ref m, _) => m.clone(),
        Poly::Quan(ref i, ref p) => rec_poly(p, &mut collect![(i.clone(), creator.create())], creator, ut),
    }
}

pub fn let_<I: Into<Ident>>(i: I, v: Box<Expr>, e: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Let(i.into(), v, e))
}

pub fn app(e1: Box<Expr>, e2: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::App(e1, e2))
}

pub fn abs<I: Into<Ident>>(i: I, e: Box<Expr>) -> Box<Expr> {
    Box::new(Expr::Abs(i.into(), e))
}

pub fn var<I: Into<Ident>>(i: I) -> Box<Expr> {
    Box::new(Expr::Var(i.into()))
}

#[test]
fn unknown_var() {
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e = var("x");
    assert_eq!(
        Err(UnboundVar("x".into())),
        check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth)
    );
}

#[test]
fn let_id() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e =
        let_(
            "x",
            abs(
                "y",
                var("y")
            ),
            var("x")
        );
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    if let Ok(ref r) = r {
        let r = ut.probe_value(*r);
        if let App(TyFun { ref name, ref params }) = r {
            assert_eq!(name, "->");
            assert_eq!(
                params.iter().map(|p| ut.probe_value(*p)).collect::<Vec<_>>(),
                vec![
                    Var("i2".into()),
                    Var("i2".into())
                ]
            );
        } else {
            panic!("Monotype wasn't right type function application: {:?}", r);
        }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}

#[test]
fn integer() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e = var("1");
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    if let Ok(ref r) = r {
        let r = ut.probe_value(*r);
        if let App(TyFun { ref name, ref params}) = r {
            assert_eq!(name, "I32");
            assert_eq!(params, &vec![]);
        } else {
            panic!("Monotype wasn't right type function application: {:?}", r);
        }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}

#[test]
fn ret_integer() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e =
        abs(
            "y",
            var("1")
        );
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    if let Ok(ref r) = r {
        let r = ut.probe_value(*r);
        if let App(TyFun { ref name, ref params }) = r {
            assert_eq!(name, "->");
            assert_eq!(
                params.iter().map(|p| ut.probe_value(*p)).collect::<Vec<_>>(),
                vec![
                    Var("i1".into()),
                    App(TyFun::integer())
                ]
            );
        } else {
            panic!("Monotype wasn't right type function application: {:?}", r);
        }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}

#[test]
fn abs_app() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e =
        app(
            abs(
                "y",
                var("y")
            ),
            var("1")
        );
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    println!("{:?}", ctx.types);
    println!("{:#?}", ut);
    if let Ok(ref r) = r {
        println!("r: {:?}", r);
        let r = ut.probe_value(*r);
        if let Var(ref i) = r {
            assert_eq!(i, "I32");
        } else {
            panic!("Monotype wasn't variable: {:?}", r);
        }
        // if let App(TyFun { ref name, ref params}) = r {
        //     assert_eq!(name, "->");
        //     assert_eq!(
        //         params.iter().map(|p| ut.probe_value(*p)).collect::<Vec<_>>(),
        //         vec![
        //             App(TyFun::integer()),
        //             App(TyFun::integer())
        //         ]
        //     );
        // } else {
        //     if let Var(ref r) = r {
        //         println!("Var: {:?}", ctx.get(r.clone()));
        //     }
        //     panic!("Monotype wasn't right type function application: {:?}", r);
        // }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}