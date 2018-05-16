use std::fmt;

use Ident;

pub enum Expr {
    Var(Ident),
    App(Box<Expr>, Box<Expr>),
    Abs(Ident, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Var(ref ident) => fmt.write_str(ident),
            App(ref lhs, ref rhs) => write!(fmt, "({}) {}", lhs, rhs),
            Abs(ref ident, ref rhs) => write!(fmt, "λ{}.{}", ident, rhs),
            Let(ref ident, ref value, ref target) => write!(fmt, "let {} = {} in {}", ident, value, target),
        }
    }
}