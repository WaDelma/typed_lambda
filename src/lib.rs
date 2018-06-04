#[macro_use]
extern crate collect_mac;
extern crate ena;
extern crate unicode_xid;

pub mod lexer;
pub mod parser;
pub mod inference;
pub mod interpreter;

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