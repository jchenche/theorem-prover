use std::fmt::Display;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Pred {
    id: String,
    args: Vec<Box<Term>>
}

#[derive(Debug)]
pub enum Term {
    Obj(Obj),
    Var(Var),
    Fun(Fun)
}

#[derive(Debug)]
pub struct Obj {
    id: String
}

#[derive(Debug)]
pub struct Var {
    id: String
}

#[derive(Debug)]
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
