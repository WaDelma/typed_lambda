#[macro_use]
extern crate collect_mac;
extern crate ena;
extern crate unicode_xid;

// use ena::unify::{UnifyKey, UnifyValue};
// use ena::unify::InPlaceUnificationTable as UnificationTable;

// use std::collections::{HashMap, HashSet};
// use std::fmt;
// use inference::{InferVar, InferenceTable};

// use self::TypecheckError::*;

pub mod lexer;
pub mod parser;
pub mod inference;
pub mod interpreter;
// mod test;

type Ident = String;

#[derive(Debug, PartialEq)]
pub enum TypecheckError {
    LetDepth,
    UnboundVar(Ident),
}

pub type Result<T> = std::result::Result<T, TypecheckError>;

#[cfg(test)]
fn parse(code: &str) -> parser::Expr {
    ::parser::Parser::new().parse(&mut ::lexer::lexer(code.chars()))
}

// // TODO: new_key is called wrongly: There is multiple ununified same things there
// pub fn check_expr(e: &Expr, ctx: &mut Ctx, creator: &mut VarCreator, ut: &mut UnificationTable<InferVar>, let_depth: usize) -> Result<InferVar> {
//     use self::Expr::*;
//     Ok(match *e {
//         Var(ref ident) => {
//             match str::parse::<i32>(ident) {
//                 Ok(_) => {
//                     let res = ut.new_key(InferVal::Bound(Mono::App(<TyFun>::integer())));
//                     println!("Varval {:?}", res);
//                     res
//                 }
//                 Err(_) => {
//                     match ctx.types.get(ident).ok_or(UnboundVar(ident.clone()))?.clone() {
//                         Poly::Mono(_, ref k) => {
//                             *k
//                         },
//                         ref ty => {
//                             let mono = inst(ty, creator, ut);
//                             let res = ut.new_key(InferVal::Bound(mono.clone()));
//                             ctx.insert(ident.clone(), Poly::Mono(mono, res));
//                             println!("Varvar {} {:?}", ident, res);
//                             res
//                         }
//                     }
//                 }
//             }
//         },
//         App(ref fun, ref params) => {
//             let newvar = ut.new_key(InferVal::Bound(Mono::Var(creator.create())));
//             println!("App1 {:?}", newvar);
//             let f_type = check_expr(fun, ctx, creator, ut, let_depth)?;
//             let p_type = check_expr(params, ctx, creator, ut, let_depth)?;
//             let fun = ut.new_key(InferVal::Bound(Mono::App(<TyFun>::new_fn(p_type, newvar))));
//             println!("App2 {:?}", fun);
//             unify(f_type, fun, ut);
//             newvar
//         },
//         Abs(ref fun, ref body) => {
//             println!("Abs1 {}", fun);
//             let newvar = Mono::Var(creator.create());
//             let param = ut.new_key(InferVal::Bound(newvar.clone()));
//             ctx.insert(fun.clone(), Poly::Mono(newvar, param));
//             println!("Abs2 {:?}", param);
//             let fun = Mono::App(TyFun::new_fn(param, check_expr(body, ctx, creator, ut, let_depth)?));
//             let res = ut.new_key(InferVal::Bound(fun));
//             println!("Abs3 {:?}", res);
//             res
//         },
//         Let(ref ident, ref value, ref exprs) => {
//             if let_depth == 0 {
//                 Err(LetDepth)?
//             }
//             let v_ty = check_expr(value, ctx, creator, ut, let_depth - 1)?;
//             let x = quantify(&ut.probe_value(v_ty), v_ty, ut);
//             ctx.insert(ident.clone(), x);
//             check_expr(exprs, ctx, creator, ut, let_depth)?
//         },
//     })
// }

// fn quantify(m: &Mono, k: InferVar, ut: &mut UnificationTable<InferVar>) -> Poly {
//     let mut result = Poly::Mono(m.clone(), k);
//     for f in m.free(ut) {
//         result = Poly::Quan(f, Box::new(result));
//     }
//     result
// }

// fn unify(a: Poly, b: Poly, ut: &mut UnificationTable<InferVar>) {
//     use Mono::*;
//     let va = ut.probe_value(a);
//     let vb = ut.probe_value(b);
//     match (va, vb) {
//         (App(TyFun { name: ref f1, params: ref v1 }), App(TyFun { name: ref f2, params: ref v2 })) if f1 == f2 && v1.len() == v2.len() => {
//             v1.iter().zip(v2.iter())
//                 .for_each(|(a, b)| unify(*a, *b, ut));
//         },
//         (Var(_), _) | (_, Var(_)) => {
//             println!("unify {:?}, {:?}", a, b);
//             ut.unify_var_var(a, b).unwrap();
//         },
//         _ => panic!("Cannot unify"),
//     }
// }

// fn inst(p: &Poly, creator: &mut VarCreator, ut: &mut UnificationTable<InferVar>) -> Mono {
//     fn rec_mono(m: &Mono, ctx: &mut HashMap<Ident, Ident>, ut: &mut UnificationTable<InferVar>) -> Mono {
//         use self::Mono::*;
//         match *m {
//             Var(ref i) => Var(ctx.get(i).cloned().unwrap_or(i.clone())),
//             App(TyFun { ref name, ref params }) => App(TyFun {
//                 name: name.clone(),
//                 params: params.iter().map(|m| {
//                     let val = ut.probe_value(*m);
//                     let mono = rec_mono(&val, ctx, ut);
//                     ut.new_key(InferVal::Bound(mono))
//                 }).collect()
//             }),
//         }
//     }
//     fn rec_poly(p: &Poly, ctx: &mut HashMap<Ident, Ident>, creator: &mut VarCreator, ut: &mut UnificationTable<InferVar>) -> Mono {
//         use self::Poly::*;
//         match *p {
//             Mono(ref m, _) => rec_mono(m, ctx, ut),
//             Quan(ref i, ref p) => {
//                 ctx.insert(i.clone(), creator.create());
//                 rec_poly(p, ctx, creator, ut)
//             }
//         }
//     }
//     match *p {
//         Poly::Mono(ref m, _) => m.clone(),
//         Poly::Quan(ref i, ref p) => rec_poly(p, &mut collect![(i.clone(), creator.create())], creator, ut),
//     }
// }

// pub fn let_<I: Into<Ident>>(i: I, v: Box<Expr>, e: Box<Expr>) -> Box<Expr> {
//     Box::new(Expr::Let(i.into(), v, e))
// }

// pub fn app(e1: Box<Expr>, e2: Box<Expr>) -> Box<Expr> {
//     Box::new(Expr::App(e1, e2))
// }

// pub fn abs<I: Into<Ident>>(i: I, e: Box<Expr>) -> Box<Expr> {
//     Box::new(Expr::Abs(i.into(), e))
// }

// pub fn var<I: Into<Ident>>(i: I) -> Box<Expr> {
//     Box::new(Expr::Var(i.into()))
// }