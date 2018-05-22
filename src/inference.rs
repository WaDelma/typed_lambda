
use std::collections::HashSet;
use std::collections::HashMap;
use std::fmt;

use ena::unify::{UnifyKey, UnifyValue};
use ena::unify::InPlaceUnificationTable as UnificationTable;

use Ident;
use parser::Expr;

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

#[derive(Clone, Debug, PartialEq)]
pub enum Poly {
    Mono(Mono),
    Quan(Ident, Box<Poly>),
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
}

pub fn inst(ty: &Poly) -> Mono {
    unimplemented!()
}

pub fn new_var() -> Mono {
    unimplemented!()
}

pub fn unify(a: &Mono, b: &Mono) {
    unimplemented!()
}

// TODO: Can this return Quan?
pub fn infer(expr: &Expr, mut ctx: Context) -> Ctx {
    use parser::ExprType::*;
    match &expr.expr {
        Error(e) => panic!("{:?}", e),
        Var(i) => {
            let ty = inst(ctx.get(i).expect("TODO"));
            ctx.insert(i.clone(), Poly::Mono(ty));
        },
        App(lhs, rhs) => {
            // TODO: This is recursion?
            let lhs_ty = if let Poly::Mono(m) = ctx.get(lhs).expect("TODO") {
                m
            } else {
                panic!("TODO");
            };
            let rhs_ty = if let Poly::Mono(m) = ctx.get(rhs).expect("TODO") {
                m
            } else {
                panic!("TODO");
            };
            let ty = new_var();
            unify(lhs_ty, &Mono::App(TyFun::new_fn(rhs_ty.clone(), ty.clone())));
            ctx.insert(App(lhs, rhs), Poly::Mono(ty));
        },
        Abs(i, body) => {
            let ty = new_var();
            let mut new_ctx = ctx.clone();
            new_ctx.insert(i, ty);
            let ret_ty = infer(body, new_ctx).get(body).clone();
            ctx.insert(Abs(i, body), ret_ty);
        },
        Let(var, val, body) => {},
    }
    ctx
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

// #[derive(Clone, Copy, PartialEq, Eq, Hash)]
// pub struct InferVar(u32);

// impl fmt::Debug for InferVar {
//     fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
//         fmt.write_str("InferVar(")?;
//         write!(fmt, "{}", self.0)?;
//         fmt.write_str(")")
//     }
// }

// impl UnifyKey for InferVar {
//     type Value = InferVal;
//     fn index(&self) -> u32 {
//         self.0
//     }
//     fn from_index(u: u32) -> Self {
//         InferVar(u)
//     }
//     fn tag() -> &'static str {
//         "TypeKey"
//     }
// }

// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
// enum InferVal {
//     Unbound(i32),
//     Bound(Mono)
// }

// impl UnifyValue for InferVal {
//     type Error = ();
//     fn unify_values<'a>(v1: &'a Self, v2: &'a Self) -> ::std::result::Result<Self, Self::Error> {
//         use self::InferVal::*;
//         Ok(match (v1, v2) {
//             (&Unbound(ref i1), &Unbound(ref i2)) => Unbound(*i1.min(i2)),
//             (b @ &Bound(_), &Unbound(_)) | (&Unbound(_), b @ &Bound(_)) => b.clone(),
//             (&Bound(_), &Bound(_)) => Err(())?,
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