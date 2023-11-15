use std::collections::HashMap;
use crate::{lang::{Var, Term, Formula, Fun, Obj}, Var, Fun, Obj};

type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<Formula>) -> Unifier {
    let mut unifier = HashMap::new();

    let processed = vec![false; formulas.len()];
    unifier.insert(Var::new("x"), Obj!("a"));
    unifier.insert(Var::new("y"), Fun!("f", [Var!("a")]));

    return unifier;
}

pub fn substitute(formula: Formula, unifier: Unifier) -> Formula {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lang::{Pred, Var, Fun, Obj}, Pred, Var, Fun, Obj};

    #[test]
    fn test_most_general_unifier_1() {
        let p_a_y = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p_x_fx = Pred!("p", [Obj!("x"), Fun!("f", [Var!("x")])]); // p(x, f(x))
        let unifier = most_general_unifier(vec![p_a_y, p_x_fx]); // Should be [x ↦ a, y ↦ f(a)]
        assert_eq!(unifier.get(&Var::new("x")), Some(&Obj!("a")));
        assert_eq!(unifier.get(&Var::new("y")), Some(&Fun!("f", [Var!("a")])));
    }
}
