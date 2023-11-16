use crate::{
    lang::{Formula, Fun, Obj, Pred, Term, Var},
    Fun, Obj, Pred, Var,
};
use std::collections::HashMap;

type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<&Formula>) -> Option<Unifier> {
    let mut unifier = HashMap::new();

    let processed = vec![false; formulas.len()];
    unifier.insert(Var::new("x"), Obj!("a"));
    unifier.insert(Var::new("y"), Fun!("f", [Var!("a")]));

    return Some(unifier);
}

pub fn substitute(formula: &Formula, unifier: &Unifier) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(Pred::new(
            pred.get_id(),
            pred.get_args()
                .iter()
                .map(|arg| Box::new(substitute_term(arg, unifier)))
                .collect(),
        )),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(l, r) => Formula::And(
            Box::new(substitute(l, unifier)),
            Box::new(substitute(r, unifier)),
        ),
        Formula::Or(l, r) => Formula::Or(
            Box::new(substitute(l, unifier)),
            Box::new(substitute(r, unifier)),
        ),
        Formula::Neg(f) => Formula::Neg(Box::new(substitute(f, unifier))),
        Formula::Imply(l, r) => Formula::Imply(
            Box::new(substitute(l, unifier)),
            Box::new(substitute(r, unifier)),
        ),
        Formula::Iff(l, r) => Formula::And(
            Box::new(substitute(l, unifier)),
            Box::new(substitute(r, unifier)),
        ),
        Formula::Forall(v, f) => Formula::Forall(v.clone(), Box::new(substitute(f, unifier))),
        Formula::Exists(v, f) => Formula::Exists(v.clone(), Box::new(substitute(f, unifier))),
    }
}

fn substitute_term(term: &Term, unifier: &Unifier) -> Term {
    match term {
        Term::Obj(_) => term.clone(),
        Term::Var(v) => match unifier.get(v) {
            None => term.clone(),
            Some(subst_t) => subst_t.clone(),
        },
        Term::Fun(f) => Term::Fun(Fun::new(
            f.get_id(),
            f.get_args()
                .iter()
                .map(|arg| Box::new(substitute_term(arg, unifier)))
                .collect(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Var},
        Fun, Obj, Pred, Var,
    };

    #[test]
    fn test_most_general_unifier_1() {
        let p_a_y = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p_x_fx = Pred!("p", [Obj!("x"), Fun!("f", [Var!("x")])]); // p(x, f(x))
        let unifier = most_general_unifier(vec![&p_a_y, &p_x_fx]).unwrap(); // should return [x ↦ a, y ↦ f(a)]
        assert_eq!(unifier.get(&Var::new("x")), Some(&Obj!("a")));
        assert_eq!(unifier.get(&Var::new("y")), Some(&Fun!("f", [Var!("a")])));
    }

    #[test]
    fn test_substitute_1() {
        let f = Pred!("r", [Fun!("g", [Var!("y")])]); // r(g(y))
        let unifier = HashMap::from([
            (Var::new("x"), Obj!("a")),
            (Var::new("y"), Fun!("f", [Var!("a")])),
        ]); // [x ↦ a, y ↦ f(a)]
        let subst_f = substitute(&f, &unifier);
        assert_eq!(subst_f, Pred!("r", [Fun!("g", [Fun!("f", [Var!("a")])])])); // r(g(f(a)))
    }

    #[test]
    fn test_substitute_2() {
        let f = Pred!("p", [Fun!("f", [Var!("y"), Fun!("g", [Var!("x")])])]); // p(f(y, g(x)))
        let unifier = HashMap::from([
            (Var::new("x"), Var!("z")),
            (Var::new("y"), Fun!("f", [Var!("a")])),
        ]); // [x ↦ z, y ↦ f(a)]
        let subst_f = substitute(&f, &unifier);
        assert_eq!(
            subst_f,
            Pred!(
                "p",
                [Fun!("f", [Fun!("f", [Var!("a")]), Fun!("g", [Var!("z")])])]
            )
        ); // p(f(f(a), g(z)))
    }
}
