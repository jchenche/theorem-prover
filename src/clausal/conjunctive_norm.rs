use crate::lang::{Formula, Term, Var};

use super::Environment;

pub fn to_cnf(formula: Formula) -> Formula {
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
    fn test_to_cnf_simple() {}
}
