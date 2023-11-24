use std::collections::{HashMap, HashSet};

use crate::lang::{Clause, Formula, Term, Var};

mod clauses_derivation;
mod conjunctive_norm;
mod prenex_norm;
mod remove_free_vars;
mod skolem_norm;

type Scope = HashMap<Var, Term>;

#[derive(Debug)]
struct Environment {
    symbol_table: Vec<Scope>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            symbol_table: vec![],
        }
    }

    pub fn get_symbol_table(&mut self) -> &mut Vec<Scope> {
        &mut self.symbol_table
    }

    pub fn push_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.symbol_table.pop();
    }

    pub fn add(&mut self, var: Var, term: Term) {
        let sym_tab = self.get_symbol_table();
        let sym_tab_len = sym_tab.len();
        sym_tab
            .get_mut(sym_tab_len - 1)
            .expect("Should always push a scope before adding a variable")
            .insert(var, term);
    }

    pub fn find(&self, var: &Var) -> Option<Term> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(term) = scope.get(var) {
                return Some(term.clone());
            }
        }
        None
    }
}

pub fn to_clausal(formula: Formula) -> Vec<Clause> {
    let sentence = remove_free_vars::remove_free_vars(formula);
    let pnf = prenex_norm::to_pnf(sentence);
    let skolem_norm = skolem_norm::skolemize(pnf);
    let cnf = conjunctive_norm::to_cnf(skolem_norm);
    let clausal_form = clauses_derivation::derive_clauses(cnf);
    return clausal_form;
}

fn get_used_bound_vars(formula: Formula) -> HashSet<String> {
    let mut used_vars = HashSet::new();
    gather_used_bound_vars(formula, &mut used_vars);
    return used_vars;
}

fn gather_used_bound_vars(formula: Formula, used_vars: &mut HashSet<String>) {
    match formula {
        Formula::Pred(_) => {}
        Formula::True => {}
        Formula::False => {}
        Formula::And(left, right) => {
            gather_used_bound_vars(*left, used_vars);
            gather_used_bound_vars(*right, used_vars);
        }
        Formula::Or(left, right) => {
            gather_used_bound_vars(*left, used_vars);
            gather_used_bound_vars(*right, used_vars);
        }
        Formula::Neg(subformula) => {
            gather_used_bound_vars(*subformula, used_vars);
        }
        Formula::Imply(left, right) => {
            gather_used_bound_vars(*left, used_vars);
            gather_used_bound_vars(*right, used_vars);
        }
        Formula::Iff(left, right) => {
            gather_used_bound_vars(*left, used_vars);
            gather_used_bound_vars(*right, used_vars);
        }
        Formula::Forall(var, subformula) => {
            used_vars.insert(var.to_string());
            gather_used_bound_vars(*subformula, used_vars);
        }
        Formula::Exists(var, subformula) => {
            used_vars.insert(var.to_string());
            gather_used_bound_vars(*subformula, used_vars);
        }
    }
}

fn find_new_var(var: &Var, used_vars: &mut HashSet<String>) -> Var {
    let mut suffix = 0;
    loop {
        let new_var = format!("{}{}", var, suffix.to_string());
        if !used_vars.contains(&new_var) {
            used_vars.insert(new_var.clone());
            return Var::new(&new_var);
        }
        suffix += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Imply, Neg, Obj, Or, Pred, Var,
    };

    #[test]
    fn test_to_clausal_complex() {
        let formula = Neg!(Neg!(Exists!(
            "y",
            Forall!(
                "z",
                Iff!(
                    Pred!("p", [Var!("z"), Var!("y")]),
                    Neg!(Exists!(
                        "x",
                        And!(
                            Pred!("p", [Var!("z"), Var!("x")]),
                            Pred!("p", [Var!("x"), Var!("z")])
                        )
                    ))
                )
            )
        ))); // ~(~(exists y.(forall z.((p(z, y)) <-> (~(exists x.((p(z, x)) /\ (p(x, z)))))))))

        let p1 = Neg!(Pred!("p", [Var!("z"), Obj!("o0")])); // ~p(z, o0)
        let p2 = Neg!(Pred!("p", [Var!("z"), Var!("x")])); // ~p(z, x)
        let p3 = Neg!(Pred!("p", [Var!("x"), Var!("z")])); // ~p(x, z)
        let p4 = Pred!("p", [Var!("z0"), Obj!("o0")]); // p(z0, o0)
        let p5 = Pred!("p", [Var!("z0"), Fun!("f0", [Var!("z0"), Var!("x0")])]); // p(z0, f0(z0, x0))
        let p6 = Pred!("p", [Var!("z1"), Obj!("o0")]); // p(z1, o0)
        let p7 = Pred!("p", [Fun!("f0", [Var!("z1"), Var!("x1")]), Var!("z1")]); // p(f0(z1), z1)

        let c1 = Clause::new(vec![p1, p2, p3]); // C1: {~p(z, o0), ~p(z, x), ~p(x, z)}
        let c2 = Clause::new(vec![p4, p5]); // C2: {p(z0, o0), p(z0, f0(z0))}
        let c3 = Clause::new(vec![p6, p7]); // C3: {p(z1, o0), p(f0(z1), z1)}

        let expected_result = vec![c1, c2, c3];
        assert_eq!(to_clausal(formula), expected_result);
    }

    #[test]
    fn test_to_clausal_1() {
        let formula = Forall!(
            "w",
            Or!(
                Neg!(Exists!(
                    "x",
                    Exists!(
                        "y",
                        Forall!(
                            "z",
                            Imply!(
                                Pred!("p", [Var!("x"), Var!("z")]),
                                Pred!("q", [Var!("y"), Var!("z")])
                            )
                        )
                    )
                )),
                Exists!("z", Pred!("p", [Var!("w"), Var!("z")]))
            )
        ); // forall w. ((~exists x. exists y. forall z. (p(x, z) -> q(y, z))) \/ exists z. p(w, z))
        let p1 = Pred!(
            "p",
            [Var!("x"), Fun!("f0", [Var!("w"), Var!("x"), Var!("y")])]
        );
        let p2 = Pred!(
            "p",
            [Var!("w"), Fun!("f1", [Var!("w"), Var!("x"), Var!("y")])]
        );
        let p3 = Neg!(Pred!(
            "q",
            [Var!("y0"), Fun!("f0", [Var!("w0"), Var!("x0"), Var!("y0")])]
        ));
        let p4 = Pred!(
            "p",
            [Var!("w0"), Fun!("f1", [Var!("w0"), Var!("x0"), Var!("y0")])]
        );
        let c1 = Clause::new(vec![p1, p2]);
        let c2 = Clause::new(vec![p3, p4]);
        let result = vec![c1, c2];
        assert_eq!(to_clausal(formula), result);
    }

    #[test]
    fn test_to_clausal_2() {
        let formula = Forall!(
            "y",
            And!(
                Pred!("p", [Var!("y")]),
                Neg!(Forall!(
                    "z",
                    Imply!(
                        Pred!("r", [Var!("z")]),
                        Pred!("q", [Var!("y"), Var!("z"), Var!("w")])
                    )
                ))
            )
        );
        let p1 = Pred!("p", [Var!("y")]);
        let p2 = Pred!("r", [Fun!("f0", [Var!("y0")])]);
        let p3 = Neg!(Pred!(
            "q",
            [Var!("y1"), Fun!("f0", [Var!("y1")]), Obj!("o0")]
        ));
        let c1 = Clause::new(vec![p1]);
        let c2 = Clause::new(vec![p2]);
        let c3 = Clause::new(vec![p3]);
        let result = vec![c1, c2, c3];
        assert_eq!(to_clausal(formula), result);
    }

    #[test]
    fn test_to_clausal_3() {
        let formula = Forall!(
            "x",
            Or!(
                Neg!(Exists!(
                    "y",
                    And!(
                        Pred!("p", [Var!("x"), Var!("y")]),
                        Pred!("p", [Var!("x"), Var!("z")])
                    )
                )),
                Exists!("y", Pred!("p", [Var!("x"), Var!("y")]))
            )
        ); // forall x.(~(exists y.(p(x, y) /\ p(x, z))) \/ exists y.(p(x, y)))

        let p1 = Neg!(Pred!("p", [Var!("x"), Var!("y")]));
        let p2 = Neg!(Pred!("p", [Var!("x"), Obj!("o0")]));
        let p3 = Pred!("p", [Var!("x"), Fun!("f0", [Var!("x"), Var!("y")])]);
        let c1 = Clause::new(vec![p1, p2, p3]);
        let result = vec![c1];
        assert_eq!(to_clausal(formula), result);
    }
}
