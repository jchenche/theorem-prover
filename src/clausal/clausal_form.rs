use crate::lang::{Formula, Term, Var, Clause};

use super::Environment;

pub fn to_clausal_form(formula: Formula) -> Vec<Clause> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Neg, Obj, Pred, Var,
    };

    #[test]
    fn test_to_clausal_form_simple() {}
}
