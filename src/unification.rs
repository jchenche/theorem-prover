use std::collections::HashMap;

use crate::lang::{Var, Term, Formula};

type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<Formula>) -> Unifier {
    todo!()
}

pub fn substitute(formula: Formula, unifier: Unifier) -> Box<Formula> {
    todo!()
}
