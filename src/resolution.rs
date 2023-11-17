use std::collections::HashSet;

use crate::{
    lang::{Clause, Formula},
    unification::{most_general_unifier, substitute},
};

pub fn resolve(mut clauses: Vec<Clause>) -> bool {
    let mut counter = 0;
    let mut resolved = Vec::new();

    // TODO: add termination logic to prevent infinite loop (e.g. time limit or max number of clauses)
    loop {
        counter += 1;
        println!("\n------ Round {} of resolution ------\n", counter);

        let mut new_clauses = Vec::new();
        for i in 0..clauses.len() {
            for j in (i + 1)..clauses.len() {
                let clause1 = clauses.get(i).unwrap();
                let clause2 = clauses.get(j).unwrap();
                println!("Attempting to resolve {} and {}", clause1, clause2);

                // check if the 2 chosen clauses have already been resolved before
                if resolved.contains(&(clause1.get_id(), clause2.get_id())) {
                    println!("Clauses have been resolved before! Skipping...");
                    continue;
                }

                let resolved_clauses = resolve_clauses(clause1, clause2);
                println!(
                    "New clauses from resolution: {}",
                    to_string(&resolved_clauses)
                );

                for clause in resolved_clauses {
                    // finish resolution if there exists an empty clause
                    if clause.get_formulas().is_empty() {
                        return false;
                    }
                    new_clauses.push(clause);
                }

                // mark 2 current clauses as resolved
                resolved.push((clause1.get_id(), clause2.get_id()));
            }
        }

        // add new clauses obtained from resolution into the vector of clauses
        clauses.append(&mut new_clauses);
        println!(
            "\n------ Set of clauses after round {} of resolution: {} ------\n",
            counter,
            to_string(&clauses)
        );
    }
}

fn resolve_clauses(clause1: &Clause, clause2: &Clause) -> Vec<Clause> {
    let mut new_clauses = Vec::new();

    // pick one formula from each clause sequentially for resolution
    for formula1 in clause1.get_formulas() {
        for formula2 in clause2.get_formulas() {
            // skip if both or none of the formulas are in the negation form
            let result = unbox_one_and_only_one_negation(formula1, formula2);
            if result.is_none() {
                // println!("{} and {} are not eligible for resolution", formula1, formula2);
                continue;
            }

            let (f1, f2) = result.unwrap();
            // unify 2 formulas if possible and then use mgu to obtain a new clause
            if let Some(unifier) = most_general_unifier(vec![f1, f2]) {
                // println!("mgu of {} and {}: {:?}", formula1, formula2, unifier);
                // use HashSet to remove duplicate formulas
                let mut new_formulas = HashSet::new();

                let substituted_formula1 = substitute(formula1, &unifier);
                let substituted_formula2 = substitute(formula2, &unifier);
                // use mgu for substitution to obtain new formulas
                for formulas in [clause1.get_formulas(), clause2.get_formulas()] {
                    for formula in formulas {
                        let substituted_formula = substitute(formula, &unifier);
                        if substituted_formula != substituted_formula1
                            && substituted_formula != substituted_formula2
                        {
                            new_formulas.insert(substituted_formula);
                        }
                    }
                }

                // create a new clause from the substituted formulas
                new_clauses.push(Clause::new(new_formulas.into_iter().collect()));
            }
        }
    }

    new_clauses
}

fn unbox_one_and_only_one_negation<'a>(
    formula1: &'a Formula,
    formula2: &'a Formula,
) -> Option<(&'a Formula, &'a Formula)> {
    if let Formula::Neg(f1) = formula1 {
        if !matches!(formula2, Formula::Neg(_)) {
            return Some((f1.as_ref(), formula2));
        }
    }

    if let Formula::Neg(f2) = formula2 {
        if !matches!(formula1, Formula::Neg(_)) {
            return Some((formula1, f2));
        }
    }

    None
}

fn to_string(clauses: &Vec<Clause>) -> String {
    let mut result = String::new();
    let len = clauses.len();
    result += "[";
    for (index, clause) in clauses.into_iter().enumerate() {
        result += &format!("{}", clause).to_string();
        if index != len - 1 {
            result += ", ";
        }
    }
    result += "]";

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Obj, Pred, Term, Var},
        Neg, Obj, Pred, Var,
    };

    #[test]
    fn test_one_and_only_one_negated() {
        let p1 = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p2 = Neg!(Pred!("p", [Obj!("x"), Var!("y")])); // ~p(x, y)

        assert!(unbox_one_and_only_one_negation(&p1, &p1).is_none());
        assert_eq!(
            (
                &Pred!("p", [Obj!("a"), Var!("y")]),
                &Pred!("p", [Obj!("x"), Var!("y")])
            ),
            unbox_one_and_only_one_negation(&p1, &p2).unwrap()
        );
        assert!(unbox_one_and_only_one_negation(&p2, &p2).is_none());
    }

    #[test]
    fn test_resolve_clauses_1() {
        let p1 = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p2 = Neg!(Pred!("p", [Var!("x"), Var!("y")])); // ~p(x, y)
        let p3 = Pred!("p", [Var!("x"), Var!("y")]); // p(x, y)

        let c1 = Clause::new(vec![p1, p3]); // C1: {p(a, y), p(x, y)}
        let c2 = Clause::new(vec![p2]); // C2: {~p(x, y)}

        let c3 = resolve_clauses(&c1, &c2);
        assert_eq!(c3.len(), 2);
        assert!(c3.get(0).unwrap().get_formulas().is_empty());
        assert_eq!(
            c3.get(1).unwrap().get_formulas(),
            &vec![Pred!("p", [Obj!("a"), Var!("y")])]
        );
    }

    #[test]
    fn test_resolve_clauses_2() {
        let p1 = Pred!("p", [Var!("x")]); // p(x)
        let p2 = Pred!("p", [Var!("y")]); // p(y)
        let p3 = Neg!(Pred!("p", [Obj!("a")])); // ~p(a)

        let c1 = Clause::new(vec![p1, p2]); // C1: {p(x), p(y)}
        let c2 = Clause::new(vec![p3]); // C2: {~p(a)}

        let c3 = resolve_clauses(&c1, &c2);
        assert_eq!(c3.len(), 2);
        assert_eq!(
            c3.get(0).unwrap().get_formulas(),
            &vec![Pred!("p", [Var!("y")])]
        );
        assert_eq!(
            c3.get(1).unwrap().get_formulas(),
            &vec![Pred!("p", [Var!("x")])]
        );
    }

    #[test]
    fn test_basic_resolve() {
        let p1 = Pred!("p", [Var!("x")]); // p(x)
        let p2 = Pred!("p", [Var!("y")]); // p(y)
        let p3 = Neg!(Pred!("p", [Obj!("a")])); // ~p(a)
        let p4 = Neg!(Pred!("p", [Obj!("b")])); // ~p(b)

        let c1 = Clause::new(vec![p1, p2]); // C1: {p(x), p(y)}
        let c2 = Clause::new(vec![p3, p4]); // C2: {~p(a), ~p(b)}

        assert!(!resolve(vec![c1, c2]));
    }
}
