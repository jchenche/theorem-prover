use crate::unification::most_general_unifier;
use crate::lang::Formula;

pub type Clause = Vec<Formula>;

pub fn to_clausal(formula: Formula) -> Vec<Clause> {
    todo!()
}
