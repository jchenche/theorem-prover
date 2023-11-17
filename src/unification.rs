use crate::lang::{Formula, Fun, Pred, Term, Var};
use std::collections::HashMap;

type Unifier = HashMap<Var, Term>;

pub fn most_general_unifier(formulas: Vec<&Formula>) -> Option<Unifier> {
    let mut predicates = vec![];
    for formula in formulas {
        match formula {
            Formula::Pred(pred) => predicates.push(pred),
            _ => unreachable!(
                "Internal State Error: it should only unify atomic formulas (aka predicates)!"
            ),
        }
    }

    if predicates.len() < 2 || !have_same_signature(&predicates) {
        return None;
    }

    let mut unifier: Unifier = HashMap::new();
    let unified_pred = predicates.get(0).unwrap();

    for predicate in &predicates {
        if !(unify_predicates(
            substitute_pred(unified_pred, &unifier),
            substitute_pred(predicate, &unifier),
            &mut unifier,
        )) {
            return None;
        }
    }

    return Some(unifier);
}

fn unify_predicates(pred1: Pred, pred2: Pred, unifier: &mut Unifier) -> bool {
    let mut pairs_to_unify = vec![];
    if !line_up_terms(pred1.get_args(), pred2.get_args(), &mut pairs_to_unify) {
        return false;
    }

    let mut i = 0;
    while i < pairs_to_unify.len() {
        match pairs_to_unify.get(i).unwrap() {
            (Term::Var(v), t) | (t, Term::Var(v)) => {
                let new_unifier = HashMap::from([(v.clone(), t.clone())]);

                pairs_to_unify.iter_mut().for_each(|(t1, t2)| {
                    *t1 = substitute_term(t1, &new_unifier);
                    *t2 = substitute_term(t2, &new_unifier);
                });

                unifier
                    .iter_mut()
                    .for_each(|(_key, val)| *val = substitute_term(val, &new_unifier));

                unifier.extend(new_unifier);
            }
            _ => unreachable!(
                "Internal State Error: it should only attempt to unify variables to terms!"
            ),
        }
        i += 1;
    }

    return true;
}

fn line_up_terms(
    args1: &Vec<Term>,
    args2: &Vec<Term>,
    pairs_to_unify: &mut Vec<(Term, Term)>,
) -> bool {
    assert!(
        args1.len() == args2.len(),
        "The formula is not in the right form. Check the arity of your symbols"
    );
    for (arg1, arg2) in args1.iter().zip(args2.iter()) {
        match (arg1, arg2) {
            (Term::Obj(o1), Term::Obj(o2)) => {
                if o1 != o2 {
                    return false; // 2 different objects can't be unified
                }
            }
            (Term::Var(v), t) | (t, Term::Var(v)) => {
                // no need to unify identical terms
                if arg1 != arg2 {
                    if term_contains_var(t, v) {
                        return false; // v and t can't be unified if t contains v (t != v at this line)
                    }
                    pairs_to_unify.push((arg1.clone(), arg2.clone()));
                }
            }
            (Term::Obj(_), Term::Fun(_)) | (Term::Fun(_), Term::Obj(_)) => return false,
            (Term::Fun(f1), Term::Fun(f2)) => {
                if f1.get_id() != f2.get_id() {
                    return false; // 2 different functions can never be unified
                } else {
                    if !line_up_terms(f1.get_args(), f2.get_args(), pairs_to_unify) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

pub fn substitute(formula: &Formula, unifier: &Unifier) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(substitute_pred(pred, unifier)),
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

fn substitute_pred(pred: &Pred, unifier: &Unifier) -> Pred {
    Pred::new(
        pred.get_id(),
        Box::new(
            pred.get_args()
                .iter()
                .map(|arg| substitute_term(arg, unifier))
                .collect(),
        ),
    )
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

fn term_contains_var(term: &Term, var: &Var) -> bool {
    match term {
        Term::Obj(_) => false,
        Term::Var(v) => v == var,
        Term::Fun(f) => f
            .get_args()
            .iter()
            .map(|arg| term_contains_var(arg, var))
            .any(|x| x),
    }
}

fn have_same_signature(predicates: &Vec<&Pred>) -> bool {
    predicates
        .iter()
        .zip(predicates.iter().skip(1))
        .all(|(p1, p2)| p1.get_id() == p2.get_id() && p1.get_args().len() == p2.get_args().len())
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
        let p1 = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p2 = Pred!("p", [Var!("x"), Fun!("f", [Var!("x")])]); // p(x, f(x))
        let unifier = most_general_unifier(vec![&p1, &p2]).unwrap();
        assert_eq!(
            unifier,
            HashMap::from([
                (Var::new("x"), Obj!("a")),
                (Var::new("y"), Fun!("f", [Obj!("a")])),
            ])
        ); // [x ↦ a, y ↦ f(a)]
    }

    #[test]
    fn test_most_general_unifier_2() {
        let p1 = Pred!(
            "p",
            [Var!("x"), Fun!("f", [Var!("x"), Fun!("g", [Var!("z")])])]
        ); // p(x, f(x, g(z)))
        let p2 = Pred!(
            "p",
            [
                Fun!("h", [Obj!("a")]),
                Fun!("f", [Var!("z"), Fun!("g", [Var!("y")])])
            ]
        ); // p(h(a), f(z, g(y)))
        let p3 = Pred!(
            "p",
            [
                Fun!("h", [Var!("w")]),
                Fun!("f", [Var!("z"), Fun!("g", [Var!("v")])])
            ]
        ); // p(h(w), f(z, g(v)))
        let unifier = most_general_unifier(vec![&p1, &p2, &p3]).unwrap();
        assert_eq!(
            unifier,
            HashMap::from([
                (Var::new("x"), Fun!("h", [Obj!("a")])),
                (Var::new("y"), Fun!("h", [Obj!("a")])),
                (Var::new("z"), Fun!("h", [Obj!("a")])),
                (Var::new("v"), Fun!("h", [Obj!("a")])),
                (Var::new("w"), Obj!("a")),
            ])
        ); // [x ↦ h(a), y ↦ h(a), z ↦ h(a), v ↦ h(a), w ↦ a]
    }

    #[test]
    fn test_most_general_unifier_3() {
        let p1 = Pred!("p", [Var!("x"), Obj!("a")]);
        let p2 = Pred!("p", [Var!("y"), Obj!("a")]);
        let p3 = Pred!("p", [Var!("z"), Obj!("a")]);
        let p4 = Pred!("p", [Fun!("f", [Var!("w")]), Obj!("a")]);
        let p5 = Pred!("p", [Fun!("f", [Obj!("a")]), Obj!("a")]);
        let unifier = most_general_unifier(vec![&p1, &p2, &p3, &p4, &p5]).unwrap();
        assert_eq!(
            unifier,
            HashMap::from([
                (Var::new("x"), Fun!("f", [Obj!("a")])),
                (Var::new("y"), Fun!("f", [Obj!("a")])),
                (Var::new("z"), Fun!("f", [Obj!("a")])),
                (Var::new("w"), Obj!("a")),
            ])
        ); // [x ↦ f(a), y ↦ f(a), z ↦ f(a), w ↦ a]
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
