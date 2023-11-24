use std::collections::{HashMap, HashSet};

use crate::{
    lang::{Clause, Formula, Fun, Pred, Term, Var},
    And, Iff, Imply, Neg, Or, Var,
};

type VarNameMap = HashMap<String, String>;

pub fn derive_clauses(formula: Formula) -> Vec<Clause> {
    let mut used_vars = super::get_used_bound_vars(formula.clone());
    let mut quantifiers = HashMap::new();
    let mut clauses = vec![vec![]];
    let no_quantifiers = drop_universal_quantifiers(formula, &mut quantifiers);
    gather_clauses(no_quantifiers, &mut clauses);
    let clauses = clauses
        .into_iter()
        .filter(|clause| !clause.is_empty())
        .collect();
    return rename_vars(clauses, &mut quantifiers, &mut used_vars)
        .iter()
        .map(|clause| Clause::new(clause.clone()))
        .collect();
}

fn drop_universal_quantifiers(formula: Formula, quantifiers: &mut VarNameMap) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(
            drop_universal_quantifiers(*left, quantifiers),
            drop_universal_quantifiers(*right, quantifiers)
        ),
        Formula::Or(left, right) => Or!(
            drop_universal_quantifiers(*left, quantifiers),
            drop_universal_quantifiers(*right, quantifiers)
        ),
        Formula::Neg(subformula) => Neg!(drop_universal_quantifiers(*subformula, quantifiers)),
        Formula::Imply(left, right) => Imply!(
            drop_universal_quantifiers(*left, quantifiers),
            drop_universal_quantifiers(*right, quantifiers)
        ),
        Formula::Iff(left, right) => Iff!(
            drop_universal_quantifiers(*left, quantifiers),
            drop_universal_quantifiers(*right, quantifiers)
        ),
        Formula::Forall(var, subformula) => {
            quantifiers.insert(var.to_string(), var.to_string());
            drop_universal_quantifiers(*subformula, quantifiers)
        }
        Formula::Exists(_, _) => {
            unreachable!("There shouldn't be any existential quantifiers after skolemization")
        }
    }
}

fn gather_clauses(formula: Formula, clauses: &mut Vec<Vec<Formula>>) {
    match formula {
        Formula::Pred(pred) => put_to_clause(Formula::Pred(pred), clauses),
        Formula::True => put_to_clause(Formula::True, clauses),
        Formula::False => put_to_clause(Formula::False, clauses),
        Formula::And(left, right) => {
            clauses.push(vec![]);
            gather_clauses(*left, clauses);
            clauses.push(vec![]);
            gather_clauses(*right, clauses);
        }
        Formula::Or(left, right) => {
            gather_clauses(*left, clauses);
            gather_clauses(*right, clauses);
        }
        Formula::Neg(subformula) => put_to_clause(Formula::Neg(subformula), clauses),
        Formula::Imply(_, _) => unreachable!("Shouldn't have any -> after CNF conversion"),
        Formula::Iff(_, _) => unreachable!("Shouldn't have any <-> after CNF conversion"),
        Formula::Forall(_, _) => {
            unreachable!("Shouldn't have any quantifiers (forall) after dropping them")
        }
        Formula::Exists(_, _) => {
            unreachable!("Shouldn't have any quantifiers (exists) after dropping them")
        }
    }
}

fn put_to_clause(formula: Formula, clauses: &mut Vec<Vec<Formula>>) {
    let last_clause_idx = clauses.len() - 1;
    clauses
        .get_mut(last_clause_idx)
        .expect("The list of clauses should not be empty before adding any formula")
        .push(formula);
}

fn rename_vars(
    clauses: Vec<Vec<Formula>>,
    quantifiers: &mut VarNameMap,
    used_vars: &mut HashSet<String>,
) -> Vec<Vec<Formula>> {
    let mut new_clauses = vec![];
    for clause in clauses {
        let mut new_clause = vec![];
        for formula in clause {
            new_clause.push(rename_vars_in_formula(formula, quantifiers, used_vars));
        }
        new_clauses.push(new_clause);
        quantifiers.iter_mut().for_each(|(key, val)| {
            *val = super::find_new_var(&Var::new(key), used_vars).to_string()
        });
    }
    return new_clauses;
}

fn rename_vars_in_formula(
    formula: Formula,
    quantifiers: &mut VarNameMap,
    used_vars: &mut HashSet<String>,
) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(Pred::new(
            pred.get_id(),
            Box::new(
                pred.get_args()
                    .iter()
                    .map(|arg| rename_vars_in_term(arg, quantifiers, used_vars))
                    .collect(),
            ),
        )),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::Neg(subformula) => {
            Neg!(rename_vars_in_formula(*subformula, quantifiers, used_vars))
        }
        _ => unreachable!(
            "Clauses should only contain atomic formulas (aka predicates) and their negation"
        ),
    }
}

fn rename_vars_in_term(
    term: &Term,
    quantifiers: &mut VarNameMap,
    used_vars: &mut HashSet<String>,
) -> Term {
    match term {
        Term::Obj(_) => term.clone(),
        Term::Var(var) => Var!(quantifiers
            .get(&var.to_string())
            .expect("All variables should've been gathered before renaming")),
        Term::Fun(f) => Term::Fun(Fun::new(
            f.get_id(),
            Box::new(
                f.get_args()
                    .iter()
                    .map(|arg| rename_vars_in_term(arg, quantifiers, used_vars))
                    .collect(),
            ),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Forall, Fun, Neg, Obj, Or, Pred, Var,
    };

    #[test]
    fn test_to_clausal_form_1() {
        let formula = Forall!(
            "y",
            And!(
                And!(
                    Pred!("p", [Var!("y")]),
                    Pred!("r", [Fun!("f", [Var!("y")])])
                ),
                Neg!(Pred!("q", [Var!("y"), Fun!("f", [Var!("y")]), Obj!("c")]))
            )
        ); // forall y (p(y) /\ r(f(y)) /\ ~q(y, f(y), c)
        let p1 = Pred!("p", [Var!("y")]);
        let p2 = Pred!("r", [Fun!("f", [Var!("y0")])]);
        let p3 = Neg!(Pred!("q", [Var!("y1"), Fun!("f", [Var!("y1")]), Obj!("c")]));
        let c1 = Clause::new(vec![p1]);
        let c2 = Clause::new(vec![p2]);
        let c3 = Clause::new(vec![p3]);
        let result = vec![c1, c2, c3];
        assert_eq!(derive_clauses(formula), result);
    }

    #[test]
    fn test_to_clausal_form_2() {
        let formula = Forall!(
            "w",
            Forall!(
                "x",
                Forall!(
                    "y",
                    And!(
                        Or!(
                            Pred!(
                                "p",
                                [Var!("x"), Fun!("g", [Var!("w"), Var!("x"), Var!("y")])]
                            ),
                            Pred!("p", [Var!("w"), Fun!("f", [Var!("w")])])
                        ),
                        Or!(
                            Neg!(Pred!(
                                "q",
                                [Var!("y"), Fun!("g", [Var!("w"), Var!("x"), Var!("y")])]
                            )),
                            Pred!("p", [Var!("w"), Fun!("f", [Var!("w")])])
                        )
                    )
                )
            )
        ); // forall w. forall. x. forall. y. ((p(x, g(w, x, y)) \/ p(w, f(w))) /\ (~q(y, g(w, x, y)) \/ p(w, f(w))))
        let p1 = Pred!(
            "p",
            [Var!("x"), Fun!("g", [Var!("w"), Var!("x"), Var!("y")])]
        );
        let p2 = Pred!("p", [Var!("w"), Fun!("f", [Var!("w")])]);
        let p3 = Neg!(Pred!(
            "q",
            [Var!("y0"), Fun!("g", [Var!("w0"), Var!("x0"), Var!("y0")])]
        ));
        let p4 = Pred!("p", [Var!("w0"), Fun!("f", [Var!("w0")])]);
        let c1 = Clause::new(vec![p1, p2]);
        let c2 = Clause::new(vec![p3, p4]);
        let result = vec![c1, c2];
        assert_eq!(derive_clauses(formula), result);
    }

    #[test]
    fn test_to_clausal_form_3() {
        let formula = Forall!(
            "z",
            Forall!(
                "x",
                And!(
                    And!(
                        Or!(
                            Or!(
                                Neg!(Pred!("p", [Var!("z"), Obj!("a")])),
                                Neg!(Pred!("p", [Var!("z"), Var!("x")]))
                            ),
                            Neg!(Pred!("p", [Var!("x"), Var!("z")]))
                        ),
                        Or!(
                            Pred!("p", [Var!("z"), Obj!("a")]),
                            Pred!("p", [Var!("z"), Fun!("f", [Var!("z")])])
                        )
                    ),
                    Or!(
                        Pred!("p", [Var!("z"), Obj!("a")]),
                        Pred!("p", [Fun!("f", [Var!("z")]), Var!("z")])
                    )
                )
            )
        );
        // forall z.(forall x.(
        //     (~p(z, a) \/ ~p(z, x) \/ ~p(x, z))
        //     /\ (p(z, a) \/ p(z, f(z)))
        //     /\ (p(z, a) \/ p(f(z), z))
        // ))

        let p1 = Neg!(Pred!("p", [Var!("z"), Obj!("a")]));
        let p2 = Neg!(Pred!("p", [Var!("z"), Var!("x")]));
        let p3 = Neg!(Pred!("p", [Var!("x"), Var!("z")]));
        let p4 = Pred!("p", [Var!("z0"), Obj!("a")]);
        let p5 = Pred!("p", [Var!("z0"), Fun!("f", [Var!("z0")])]);
        let p6 = Pred!("p", [Var!("z1"), Obj!("a")]);
        let p7 = Pred!("p", [Fun!("f", [Var!("z1")]), Var!("z1")]);

        let c1 = Clause::new(vec![p1, p2, p3]);
        let c2 = Clause::new(vec![p4, p5]);
        let c3 = Clause::new(vec![p6, p7]);

        let result = vec![c1, c2, c3];
        assert_eq!(derive_clauses(formula), result);
    }
}
