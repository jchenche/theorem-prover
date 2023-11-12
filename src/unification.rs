use std::collections::HashMap;

use crate::lang::{Var, Term, Formula};

pub type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<Formula>) -> Unifier {
    todo!()
}
