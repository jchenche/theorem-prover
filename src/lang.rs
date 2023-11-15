use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Formula {
    Pred(Pred),
    True,
    False,
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    Neg(Box<Formula>),
    Imply(Box<Formula>, Box<Formula>),
    Iff(Box<Formula>, Box<Formula>),
    Forall(Var, Box<Formula>),
    Exists(Var, Box<Formula>)
}

#[derive(Debug, Clone)]
pub struct Pred {
    id: String,
    args: Vec<Box<Term>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Obj(Obj),
    Var(Var),
    Fun(Fun)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Obj {
    id: String
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Var {
    id: String
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    id: String,
    args: Vec<Box<Term>>
}

impl Pred {
    pub fn new(id: &str, args: Vec<Box<Term>>) -> Self {
        Self { id: id.to_string(), args }
    }
}

impl Obj {
    pub fn new(id: &str) -> Self {
        Self { id: id.to_string()}
    }
}

impl Var {
    pub fn new(id: &str) -> Self {
        Self { id: id.to_string()}
    }
}

impl Fun {
    pub fn new(id: &str, args: Vec<Box<Term>>) -> Self {
        Self { id: id.to_string(), args }
    }
}

impl Display for Formula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Formula::Pred(pred) => write!(f, "{pred}"),
            Formula::True => write!(f, "true"),
            Formula::False => write!(f, "false"),
            Formula::And(l, r) => write!(f, "({l}) ∧ ({r})"),
            Formula::Or(l, r) => write!(f, "({l}) v ({r})"),
            Formula::Neg(l) => write!(f, "¬({l})"),
            Formula::Imply(l, r) => write!(f, "({l}) → ({r})"),
            Formula::Iff(l, r) => write!(f, "({l}) ↔ ({r})"),
            Formula::Forall(v, l) => write!(f, "∀{v}.({l})"),
            Formula::Exists(v, l) => write!(f, "∃{v}.({l})"),
        }
    }
}

impl Display for Pred {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.id)?;
        for (index, arg) in self.args.iter().enumerate() {
            write!(f, "{arg}")?;
            if index != self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Term::Obj(Obj { id }) => write!(f, "{id}"),
            Term::Var(Var { id }) => write!(f, "{id}"),
            Term::Fun(func) => write!(f, "{func}"),
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }   
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }   
}

impl Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.id)?;
        for (index, arg) in self.args.iter().enumerate() {
            write!(f, "{arg}")?;
            if index != self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

#[macro_export]
macro_rules! And {
    ($left:expr, $right:expr) => {
        Formula::And(Box::new($left), Box::new($right))
    };
}

#[macro_export]
macro_rules! Or {
    ($left:expr, $right:expr) => {
        Formula::Or(Box::new($left), Box::new($right))
    };
}

#[macro_export]
macro_rules! Imply {
    ($left:expr, $right:expr) => {
        Formula::Imply(Box::new($left), Box::new($right))
    };
}

#[macro_export]
macro_rules! Iff {
    ($left:expr, $right:expr) => {
        Formula::Iff(Box::new($left), Box::new($right))
    };
}

#[macro_export]
macro_rules! Neg {
    ($subformula:expr) => {
        Formula::Neg(Box::new($subformula))
    };
}

#[macro_export]
macro_rules! Forall {
    ($var:expr, $subformula:expr) => {
        Formula::Forall(Var::new($var), Box::new($subformula))
    };
}

#[macro_export]
macro_rules! Exists {
    ($var:expr, $subformula:expr) => {
        Formula::Exists(Var::new($var), Box::new($subformula))
    };
}

#[macro_export]
macro_rules! Pred {
    ($id:expr, [$($arg:expr),*]) => {
        Formula::Pred(Pred::new($id, vec![$(Box::new($arg)),*]))
    };
}

#[macro_export]
macro_rules! Obj {
    ($name:expr) => {
        Term::Obj(Obj::new($name))
    };
}

#[macro_export]
macro_rules! Var {
    ($name:expr) => {
        Term::Var(Var::new($name))
    };
}

#[macro_export]
macro_rules! Fun {
    ($id:expr, [$($arg:expr),*]) => {
        Term::Fun(Fun::new($id, vec![$(Box::new($arg)),*]))
    };
}
