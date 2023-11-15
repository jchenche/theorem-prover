use crate::{
    lang::Clause,
    unification::{most_general_unifier, substitute},
};

pub fn resolve(mut clauses: Vec<Clause>) -> bool {
    let mut resolved = Vec::new();

    loop {
        let mut new_clauses = Vec::new();

        for i in 0..clauses.len() {
            for j in (i + 1)..clauses.len() {
                let clause1 = clauses.get(i).unwrap();
                let clause2 = clauses.get(j).unwrap();

                // check if the 2 chosen clauses have already been resolved before
                if resolved.contains(&(clause1.get_id(), clause2.get_id())) {
                    continue;
                }

                for clause in resolve_clauses(clause1, clause2) {
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
    }
}

fn resolve_clauses(clause1: &Clause, clause2: &Clause) -> Vec<Clause> {
    let mut new_clauses = Vec::new();

    // pick one formula from each clause sequentially for resolution
    for formula1 in clause1.get_formulas() {
        for formula2 in clause2.get_formulas() {
            // unify 2 formulas if possible and then use mgu to obtain a new clause
            if let Some(unifier) = most_general_unifier(vec![formula1, formula2]) {
                let mut new_formulas = Vec::new();

                // use mgu for substitution to obtain new formulas
                for formula in clause1.get_formulas() {
                    if formula != formula1 {
                        new_formulas.push(substitute(formula, &unifier));
                    }
                }
                for formula in clause2.get_formulas() {
                    if formula != formula2 {
                        new_formulas.push(substitute(formula, &unifier));
                    }
                }

                // create a new clause from the substituted formulas
                new_clauses.push(Clause::new(new_formulas));
            }
        }
    }

    new_clauses
}
