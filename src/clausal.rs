use std::collections::{HashMap, HashSet};

use crate::lang::{Clause, Formula, Term, Var};

mod remove_free_vars;

type Scope = HashSet<Var>;

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
        self.symbol_table.push(HashSet::new());
    }

    pub fn pop_scope(&mut self) {
        self.symbol_table.pop();
    }

    pub fn add(&mut self, var: Var) {
        let sym_tab = self.get_symbol_table();
        let sym_tab_len = sym_tab.len();
        sym_tab.get_mut(sym_tab_len - 1).unwrap().insert(var);
    }

    pub fn find(&self, var: &Var) -> Option<Var> {
        for scope in self.symbol_table.iter().rev() {
            if scope.contains(var) {
                return Some(var.clone());
            }
        }
        None
    }
}

pub fn to_clausal(formula: Formula) -> Vec<Clause> {
    let sentence = remove_free_vars::remove_free_vars(formula);
    let pnf = to_pnf(sentence);
    let skolem_norm = skolemize(pnf);
    let cnf = to_cnf(skolem_norm);
    let clausal_form = to_clausal_form(cnf);
    return clausal_form;
}

fn to_pnf(formula: Formula) -> Formula {
    todo!()
}

fn skolemize(formula: Formula) -> Formula {
    todo!()
}

fn to_cnf(formula: Formula) -> Formula {
    todo!()
}

fn to_clausal_form(formula: Formula) -> Vec<Clause> {
    todo!()
}

fn template(formula: Formula) {
    match formula {
        Formula::Pred(pred) => todo!(),
        Formula::True => todo!(),
        Formula::False => todo!(),
        Formula::And(left, right) => todo!(),
        Formula::Or(left, right) => todo!(),
        Formula::Neg(subformula) => todo!(),
        Formula::Imply(left, right) => todo!(),
        Formula::Iff(left, right) => todo!(),
        Formula::Forall(var, subformula) => todo!(),
        Formula::Exists(var, subformula) => todo!(),
    }
}

// example transformation
fn and_to_or(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(l, r) => Formula::Or(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Or(l, r) => Formula::Or(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Neg(f) => Formula::Neg(Box::new(and_to_or(*f))),
        Formula::Imply(l, r) => Formula::Imply(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Iff(l, r) => Formula::Iff(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Forall(v, f) => Formula::Forall(v, Box::new(and_to_or(*f))),
        Formula::Exists(v, f) => Formula::Exists(v, Box::new(and_to_or(*f))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Neg, Obj, Pred, Var,
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
}
