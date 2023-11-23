use std::collections::{HashMap, HashSet};

use crate::lang::{Clause, Formula, Term, Var};

mod clausal_form;
mod conjunctive_norm;
mod prenex_norm;
mod remove_free_vars;
mod skolem_norm;

type Scope = HashMap<Var, Term>;

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
        sym_tab.get_mut(sym_tab_len - 1).unwrap().insert(var, term);
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
    let mut used_vars = get_used_bound_vars(sentence.clone());
    let pnf = prenex_norm::to_pnf(sentence, &mut used_vars);
    let skolem_norm = skolem_norm::skolemize(pnf);
    let cnf = conjunctive_norm::to_cnf(skolem_norm);
    let clausal_form = clausal_form::to_clausal_form(cnf);
    return clausal_form;
}

fn get_used_bound_vars(formula: Formula) -> HashSet<Var> {
    let mut used_vars = HashSet::new();
    gather_used_bound_vars(formula, &mut used_vars);
    return used_vars;
}

fn gather_used_bound_vars(formula: Formula, used_vars: &mut HashSet<Var>) {
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
            used_vars.insert(var);
            gather_used_bound_vars(*subformula, used_vars);
        }
        Formula::Exists(var, subformula) => {
            used_vars.insert(var);
            gather_used_bound_vars(*subformula, used_vars);
        }
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
    fn test_to_clausal() {
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
            [Var!("x"), Fun!("f1", [Var!("w"), Var!("x"), Var!("y")])]
        );
        let p2 = Pred!("p", [Var!("w"), Fun!("f0", [Var!("w")])]);
        let p3 = Neg!(Pred!(
            "q",
            [Var!("y0"), Fun!("f1", [Var!("w0"), Var!("x0"), Var!("y0")])]
        ));
        let p4 = Pred!("p", [Var!("w0"), Fun!("f0", [Var!("w0")])]);
        let c1 = Clause::new(vec![p1, p2]);
        let c2 = Clause::new(vec![p3, p4]);
        let result = vec![c1, c2];
        assert_eq!(result, to_clausal(formula));
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
        assert_eq!(result, to_clausal(formula));
    }
}
