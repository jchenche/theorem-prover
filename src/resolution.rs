use crate::{
    lang::{Clause, Formula},
    unification::{most_general_unifier, substitute},
};
use std::{
    collections::HashSet,
    time::{Duration, Instant},
};

pub fn refute_resolution(mut clauses: Vec<Clause>, limit_in_seconds: u64) -> Option<bool> {
    let mut counter = 0;
    let mut resolved = Vec::new();
    let start = Instant::now();

    loop {
        counter += 1;
        println!("\n------ Round {} of resolution ------\n", counter);

        let mut new_clauses = Vec::new();
        for i in 0..clauses.len() {
            for j in (i + 1)..clauses.len() {
                // end the resolution process if it has already exceeded the time limit
                if Instant::now() - start >= Duration::from_secs(limit_in_seconds) {
                    println!(
                        "\n------ Unable to determine whether there is a resolution refutation within the time limit ------ \n"
                    );
                    return None;
                }

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
                        println!(
                            "\n------ Successfully found a resolution refutation {} ------\n",
                            clause
                        );
                        return Some(true);
                    }
                    new_clauses.push(clause);
                }

                // mark 2 current clauses as resolved
                resolved.push((clause1.get_id(), clause2.get_id()));
            }
        }

        if new_clauses.is_empty() {
            println!("\n------ No resolution refutation as there is no new clause after a round of resolution ------ \n");
            return Some(false);
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

    // pick one formula from each clause iteratively for resolution
    for formula1 in clause1.get_formulas() {
        for formula2 in clause2.get_formulas() {
            // ensure that one formula is in the negation form whereas the other is not
            let result = undo_one_and_only_one_negation(formula1, formula2);
            if result.is_none() {
                // println!("{} and {} are not eligible for resolution", formula1, formula2);

                continue;
            }

            // (~f1, f2) = (formula1, formula2) OR (f1, ~f2) = (formula1, formula2)
            // f1 and f2 are in the non-negation form
            let (f1, f2) = result.unwrap();
            // unify the 2 formulas and then use mgu (if possible) to derive a new clause
            if let Some(unifier) = most_general_unifier(vec![f1, f2]) {
                // println!("mgu of {} and {}: {:?}", formula1, formula2, unifier);

                // use HashSet to remove duplicate formulas
                let mut new_formulas = HashSet::new();

                // perform substitution and filter out resolved formulas to build a new set of formulas
                let substituted_formula1 = substitute(formula1, &unifier);
                for formula in clause1.get_formulas() {
                    let substituted_formula = substitute(formula, &unifier);
                    if substituted_formula != substituted_formula1 {
                        new_formulas.insert(substituted_formula);
                    }
                }
                let substituted_formula2 = substitute(formula2, &unifier);
                for formula in clause2.get_formulas() {
                    let substituted_formula = substitute(formula, &unifier);
                    if substituted_formula != substituted_formula2 {
                        new_formulas.insert(substituted_formula);
                    }
                }

                // derive a new clause from the new set of formulas
                new_clauses.push(Clause::new(new_formulas.into_iter().collect()));
            }
        }
    }

    new_clauses
}

fn undo_one_and_only_one_negation<'a>(
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
        lang::{Fun, Obj, Pred, Term, Var},
        Fun, Neg, Obj, Pred, Var,
    };

    const DEFAULT_LIMIT: u64 = 60;

    #[test]
    fn test_undo_one_and_only_one_negation() {
        let p1 = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p2 = Neg!(Pred!("p", [Obj!("x"), Var!("y")])); // ~p(x, y)

        assert!(undo_one_and_only_one_negation(&p1, &p1).is_none());
        assert_eq!(
            (
                &Pred!("p", [Obj!("a"), Var!("y")]),
                &Pred!("p", [Obj!("x"), Var!("y")])
            ),
            undo_one_and_only_one_negation(&p1, &p2).unwrap()
        );
        assert!(undo_one_and_only_one_negation(&p2, &p2).is_none());
    }

    #[test]
    fn test_resolve_clauses_1() {
        let p1 = Pred!("p", [Obj!("a"), Var!("y")]); // p(a, y)
        let p2 = Neg!(Pred!("p", [Var!("x"), Var!("y")])); // ~p(x, y)
        let p3 = Pred!("p", [Var!("x"), Var!("y")]); // p(x, y)

        let c1 = Clause::new(vec![p1, p3]); // C1: {p(a, y), p(x, y)}
        let c2 = Clause::new(vec![p2]); // C2: {~p(x, y)}

        let c3 = Clause::new(vec![]); // C3: {}
        let c4 = Clause::new(vec![Pred!("p", [Obj!("a"), Var!("y")])]); // C4: {p(a, y)}

        assert_eq!(vec![c3, c4], resolve_clauses(&c1, &c2));
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
    fn test_basic_resolution_refutation_1() {
        let p1 = Pred!("happy", [Var!("x")]); // happy(x)
        let p2 = Pred!("sad", [Var!("x")]); // sad(x)
        let p3 = Neg!(Pred!("sad", [Var!("y")])); // ~sad(y)
        let p4 = Neg!(Pred!("happy", [Fun!("mother", [Obj!("Joe")])])); // ~happy(mother(Joe))

        let c1 = Clause::new(vec![p1, p2]); // C1: {happy(x), sad(x)}
        let c2 = Clause::new(vec![p3]); // C2: {~sad(y)}
        let c3 = Clause::new(vec![p4]); // C3: {~happy(mother(Joe))}

        assert!(refute_resolution(vec![c1, c2, c3], DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_basic_resolution_refutation_2() {
        let p1 = Pred!("p", [Var!("x")]); // p(x)
        let p2 = Pred!("p", [Var!("y")]); // p(y)
        let p3 = Neg!(Pred!("p", [Obj!("a")])); // ~p(a)
        let p4 = Neg!(Pred!("p", [Obj!("b")])); // ~p(b)

        let c1 = Clause::new(vec![p1, p2]); // C1: {p(x), p(y)}
        let c2 = Clause::new(vec![p3, p4]); // C2: {~p(a), ~p(b)}

        assert!(refute_resolution(vec![c1, c2], DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_basic_resolution_refutation_3() {
        let p1 = Neg!(Pred!("p", [Var!("z"), Obj!("a")])); // ~p(z, a)
        let p2 = Neg!(Pred!("p", [Var!("z"), Var!("x")])); // ~p(z, x)
        let p3 = Neg!(Pred!("p", [Var!("x"), Var!("z")])); // ~p(x, z)
        let p4 = Pred!("p", [Var!("y"), Obj!("a")]); // p(y, a)
        let p5 = Pred!("p", [Var!("y"), Fun!("f", [Var!("y")])]); // p(y, f(y))
        let p6 = Pred!("p", [Var!("w"), Obj!("a")]); // p(w, a)
        let p7 = Pred!("p", [Fun!("f", [Var!("w")]), Var!("w")]); // p(f(w), w)

        let c1 = Clause::new(vec![p1, p2, p3]); // C1: {~p(z, a), ~p(z, x), ~p(x, z)}
        let c2 = Clause::new(vec![p4, p5]); // C2: {p(y, a), p(y, f(y))}
        let c3 = Clause::new(vec![p6, p7]); // C3: {p(y, a), p(f(w), w)}

        assert!(refute_resolution(vec![c1, c2, c3], DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_basic_resolution_refutation_4() {
        let p1 = Pred!("loves", [Var!("x"), Fun!("lover", [Var!("x")])]); // loves(x, lover(x))
        let p2 = Neg!(Pred!("loves", [Var!("y"), Var!("z")])); // ~loves(y, z)
        let p3 = Pred!("loves", [Var!("w"), Var!("y")]); // loves(w, y)
        let p4 = Neg!(Pred!("loves", [Obj!("Joe"), Obj!("Jane")])); // ~loves(Joe, Jane)

        let c1 = Clause::new(vec![p1]); // C1: {loves(x, lover(x))}
        let c2 = Clause::new(vec![p2, p3]); // C2: {~loves(y, z), loves(w, y)}
        let c3 = Clause::new(vec![p4]); // C3: {~loves(Joe, Jane)}

        assert!(refute_resolution(vec![c1, c2, c3], DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_basic_resolution_refutation_5() {
        let p1 = Neg!(Pred!("p", [Var!("x")])); // ~p(x)
        let p2 = Pred!("q", [Var!("x")]); // q(x)
        let p3 = Neg!(Pred!("q", [Var!("y")])); // ~q(y)
        let p4 = Pred!("p", [Var!("y")]); // p(y)

        let c1 = Clause::new(vec![p1, p2]); // C1: {~p(x), q(x)}
        let c2 = Clause::new(vec![p3, p4]); // C2: {~q(y), p(y)}

        assert!(refute_resolution(vec![c1, c2], 5).is_none());
    }

    #[test]
    fn test_basic_resolution_refutation_6() {
        let p1 = Neg!(Pred!("p", [Var!("x")])); // ~p(x)
        let p2 = Pred!("q", [Var!("x")]); // q(x)
        let p3 = Neg!(Pred!("q", [Var!("x")])); // ~q(x)

        let c1 = Clause::new(vec![p1, p2]); // C1: {~p(x), q(x)}
        let c2 = Clause::new(vec![p3]); // C2: {~q(x)}

        assert!(!refute_resolution(vec![c1, c2], DEFAULT_LIMIT).unwrap());
    }
}
