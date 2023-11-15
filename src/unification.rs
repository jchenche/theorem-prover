use std::collections::HashMap;
use crate::lang::{Var, Term, Formula};

type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<Formula>) -> Unifier {
    let processed = vec![false; formulas.len()];
    todo!()
}

pub fn substitute(formula: Formula, unifier: Unifier) -> Formula {
    todo!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_most_general_unifier_1() {

    }
}
