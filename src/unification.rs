use crate::{
    lang::{Formula, Fun, Obj, Pred, Term, Var},
    Fun, Obj, Pred, Var,
};
use std::collections::HashMap;

type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<&Formula>) -> Option<Unifier> {
    let mut predicates = vec![];
    for formula in formulas {
        match formula {
            Formula::Pred(pred) => predicates.push(pred),
            _ => return None, // Only consider predicates
        }
    }

    if predicates.len() < 2 || !have_same_signature(&predicates) {
        return None;
    }

    let arity = predicates.get(0).unwrap().get_args().len();
    let processed = vec![false; arity];
    let mut nth_args = vec![vec![]; arity];
    let mut unifier = HashMap::new();

    for predicate in predicates {
        for (ith, term) in predicate.get_args().iter().enumerate() {
            nth_args[ith].push(term);
        }
    }

    for ith_args in &nth_args {
        for term in ith_args {
            print!("{}, ", term);
        }
        println!();
    }

    for ith_args in nth_args {
        for term in ith_args {
            if is_ground_term(term) {}
        }
    }

    // unifier.insert(Var::new("x"), Obj!("a"));
    // unifier.insert(Var::new("y"), Fun!("f", [Var!("a")]));
    return Some(unifier);
}

pub fn substitute(formula: &Formula, unifier: &Unifier) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(Pred::new(
            pred.get_id(),
            Box::new(
                pred.get_args()
                    .iter()
                    .map(|arg| substitute_term(arg, unifier))
                    .collect(),
            ),
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

fn is_ground_term(term: &Term) -> bool {
    match term {
        Term::Obj(_) => true,
        Term::Var(_) => false,
        Term::Fun(f) => f
            .get_args()
            .iter()
            .map(|arg| is_ground_term(arg))
            .all(|x| x),
    }
}

fn have_same_signature(predicates: &Vec<&Pred>) -> bool {
    // Imperative version
    // if predicates.len() == 0 {
    //     return true;
    // }

    // let pred_name = predicates.get(0).unwrap().get_id();
    // let pred_arity = predicates.get(0).unwrap().get_args().len();
    // for predicate in predicates {
    //     if predicate.get_id() != pred_name || predicate.get_args().len() != pred_arity {
    //         return false
    //     }
    // }

    // return true;

    predicates
        .iter()
        .zip(predicates.iter().skip(1))
        .all(|(p1, p2)| p1.get_id() == p2.get_id() && p1.get_args().len() == p2.get_args().len())
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
            Box::new(
                f.get_args()
                    .iter()
                    .map(|arg| substitute_term(arg, unifier))
                    .collect(),
            ),
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
