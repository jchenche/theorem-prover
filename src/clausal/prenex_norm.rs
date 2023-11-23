use std::collections::HashSet;

use crate::{
    lang::{Formula, Var, Pred, Term, Fun},
    And, Exists, Iff, Imply, Neg, Or,
};

use super::Environment;

pub fn to_pnf(formula: Formula, used_vars: &mut HashSet<Var>) -> Formula {
    let nnf = to_nnf(formula);
    let mut seen_vars = HashSet::new();
    let mut env = Environment::new();
    let bound_vars_renamed = rename_bound_vars(nnf, &mut env, &mut seen_vars, used_vars);
    todo!()
}

fn to_nnf(formula: Formula) -> Formula {
    let no_imply = eliminate_imply(formula);
    let no_iff = eliminate_iff(no_imply);
    let demorganed = apply_demorgan(no_iff);
    return demorganed;
}

fn eliminate_imply(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(eliminate_imply(*left), eliminate_imply(*right)),
        Formula::Or(left, right) => Or!(eliminate_imply(*left), eliminate_imply(*right)),
        Formula::Neg(subformula) => Neg!(eliminate_imply(*subformula)),
        Formula::Imply(left, right) => Or!(Neg!(eliminate_imply(*left)), eliminate_imply(*right)),
        Formula::Iff(left, right) => Iff!(eliminate_imply(*left), eliminate_imply(*right)),
        Formula::Forall(var, subformula) => {
            Formula::Forall(var, Box::new(eliminate_imply(*subformula)))
        }
        Formula::Exists(var, subformula) => {
            Formula::Exists(var, Box::new(eliminate_imply(*subformula)))
        }
    }
}

fn eliminate_iff(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(eliminate_iff(*left), eliminate_iff(*right)),
        Formula::Or(left, right) => Or!(eliminate_iff(*left), eliminate_iff(*right)),
        Formula::Neg(subformula) => Neg!(eliminate_iff(*subformula)),
        Formula::Imply(left, right) => Imply!(eliminate_iff(*left), eliminate_iff(*right)),
        Formula::Iff(left, right) => {
            let left = eliminate_iff(*left);
            let right = eliminate_iff(*right);
            And!(
                Or!(Neg!(left.clone()), right.clone()),
                Or!(left, Neg!(right))
            )
        }
        Formula::Forall(var, subformula) => {
            Formula::Forall(var, Box::new(eliminate_iff(*subformula)))
        }
        Formula::Exists(var, subformula) => {
            Formula::Exists(var, Box::new(eliminate_iff(*subformula)))
        }
    }
}

fn apply_demorgan(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(apply_demorgan(*left), apply_demorgan(*right)),
        Formula::Or(left, right) => Or!(apply_demorgan(*left), apply_demorgan(*right)),
        Formula::Neg(subformula) => match *subformula {
            Formula::And(left, right) => {
                Or!(apply_demorgan(Neg!(*left)), apply_demorgan(Neg!(*right)))
            }
            Formula::Or(left, right) => {
                And!(apply_demorgan(Neg!(*left)), apply_demorgan(Neg!(*right)))
            }
            Formula::Neg(subformula) => apply_demorgan(*subformula),
            Formula::Forall(var, subformula) => {
                Formula::Exists(var, Box::new(apply_demorgan(Neg!(*subformula))))
            }
            Formula::Exists(var, subformula) => {
                Formula::Forall(var, Box::new(apply_demorgan(Neg!(*subformula))))
            }
            _ => Neg!(apply_demorgan(*subformula)),
        },
        Formula::Imply(left, right) => Imply!(apply_demorgan(*left), apply_demorgan(*right)),
        Formula::Iff(left, right) => Iff!(apply_demorgan(*left), apply_demorgan(*right)),
        Formula::Forall(var, subformula) => {
            Formula::Forall(var, Box::new(apply_demorgan(*subformula)))
        }
        Formula::Exists(var, subformula) => {
            Formula::Exists(var, Box::new(apply_demorgan(*subformula)))
        }
    }
}

fn rename_bound_vars(
    formula: Formula,
    env: &mut Environment,
    seen_var: &mut HashSet<Var>,
    used_vars: &mut HashSet<Var>,
) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(Pred::new(
            pred.get_id(),
            Box::new(
                pred.get_args()
                    .iter()
                    .map(|arg| rename_bound_vars_in_terms(arg, env, seen_var, used_vars))
                    .collect(),
            ),
        )),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(rename_bound_vars(*left, env, seen_var, used_vars), rename_bound_vars(*right, env, seen_var, used_vars)),
        Formula::Or(left, right) => Or!(rename_bound_vars(*left, env, seen_var, used_vars), rename_bound_vars(*right, env, seen_var, used_vars)),
        Formula::Neg(subformula) => Neg!(rename_bound_vars(*subformula, env, seen_var, used_vars)),
        Formula::Imply(left, right) => Imply!(rename_bound_vars(*left, env, seen_var, used_vars), rename_bound_vars(*right, env, seen_var, used_vars)),
        Formula::Iff(left, right) => Iff!(rename_bound_vars(*left, env, seen_var, used_vars), rename_bound_vars(*right, env, seen_var, used_vars)),
        Formula::Forall(var, subformula) => {
            if seen_var.contains(&var) {
                let new_var = find_new_var(&var, used_vars);
                env.push_scope();
                env.add(var, Term::Var(new_var.clone()));
                let subformula = rename_bound_vars(*subformula, env, seen_var, used_vars);
                env.pop_scope();
                Formula::Forall(new_var, Box::new(subformula))
            } else {
                Formula::Forall(var, Box::new(rename_bound_vars(*subformula, env, seen_var, used_vars)))
            }
        },
        Formula::Exists(var, subformula) => {
            if seen_var.contains(&var) {
                let new_var = find_new_var(&var, used_vars);
                env.push_scope();
                env.add(var, Term::Var(new_var.clone()));
                let subformula = rename_bound_vars(*subformula, env, seen_var, used_vars);
                env.pop_scope();
                Formula::Exists(new_var, Box::new(subformula))
            } else {
                Formula::Exists(var, Box::new(rename_bound_vars(*subformula, env, seen_var, used_vars)))
            }
        },
    }
}

fn find_new_var(var: &Var, used_vars: &mut HashSet<Var>) -> Var {
    let mut suffix = 0;
    loop {
        let new_var = format!("{}{}", var, suffix.to_string());
        if !used_vars.contains(&Var::new(&new_var)) {
            return Var::new(&new_var);
        }
        suffix += 1;
    }
}

fn rename_bound_vars_in_terms(
    term: &Term,
    env: &mut Environment,
    seen_var: &mut HashSet<Var>,
    used_vars: &mut HashSet<Var>,
) -> Term {
    match term {
        Term::Obj(_) => term.clone(),
        Term::Var(v) => {
            if let Some(new_term) = env.find(v) {
                new_term
            } else {
                term.clone()
            }
        },
        Term::Fun(f) => Term::Fun(Fun::new(
            f.get_id(),
            Box::new(
                f.get_args()
                    .iter()
                    .map(|arg| rename_bound_vars_in_terms(arg, env, seen_var, used_vars))
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
        And, Exists, Forall, Fun, Iff, Neg, Obj, Or, Pred, Var,
    };

    #[test]
    fn test_to_nnf_no_quantifier() {
        let formula = Neg!(Imply!(
            Pred!("p", [Var!("x")]),
            And!(Pred!("q", [Var!("y")]), Pred!("r", [Obj!("a")]))
        )); // ~(p(x) -> (q(y) /\ r(a)))
        let expected_result = And!(
            Pred!("p", [Var!("x")]),
            Or!(Neg!(Pred!("q", [Var!("y")])), Neg!(Pred!("r", [Obj!("a")])))
        ); // p(x) /\ (~q(y) \/ ~r(a))
        assert_eq!(to_nnf(formula), expected_result);
    }

    #[test]
    fn test_to_nnf_complex() {
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
        ); // forall x . (~(exists y . (p(x, y) /\ p(x, z))) \/ exists y . p(x, y))
        let expected_result = Forall!(
            "x",
            Or!(
                Forall!(
                    "y",
                    Or!(
                        Neg!(Pred!("p", [Var!("x"), Var!("y")])),
                        Neg!(Pred!("p", [Var!("x"), Var!("z")]))
                    )
                ),
                Exists!("y", Pred!("p", [Var!("x"), Var!("y")]))
            )
        ); // forall x . ((forall y . (~p(x, y) \/ ~p(x, z))) \/ exists y . p(x, y))
        assert_eq!(to_nnf(formula), expected_result);
    }

    #[test]
    fn test_to_pnf() {
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
        ); // forall x . (~(exists y . (p(x, y) /\ p(x, z))) \/ exists y . p(x, y))
        let expected_result = Forall!(
            "x",
            Forall!(
                "y",
                Exists!(
                    "w",
                    Or!(
                        Or!(
                            Neg!(Pred!("p", [Var!("x"), Var!("y")])),
                            Neg!(Pred!("p", [Var!("x"), Var!("z")]))
                        ),
                        Pred!("p", [Var!("x"), Var!("w")])
                    )
                )
            )
        ); // forall x . (forall y . (exists w. ((~p(x, y) \/ ~p(x, z)) \/ p(x,w))))
        let mut used_vars = HashSet::from([Var::new("x"), Var::new("y"), Var::new("z")]);
        assert_eq!(to_pnf(formula, &mut used_vars), expected_result);
    }

    #[test]
    fn test_to_pnf_1() {
        let formula = Exists!(
            "w",
            Forall!(
                "y",
                And!(
                    Pred!("p", [Var!["y"]]),
                    Neg!(Forall!(
                        "z",
                        Imply!(
                            Pred!("r", [Var!("z")]),
                            Pred!("q", [Var!("y"), Var!("z"), Var!("w")])
                        )
                    ))
                )
            )
        ); //exists w. forall y. (p(y) /\ ~(forall z. (r(z) -> q(y, z, w))))
        let result_formula = Exists!(
            "w",
            Forall!(
                "y",
                Exists!(
                    "z",
                    And!(
                        And!(Pred!("p", [Var!("y")]), Pred!("r", [Var!("z")])),
                        Neg!(Pred!("q", [Var!("y"), Var!("z"), Var!("w")]))
                    )
                )
            )
        ); //exists w. forall y. exists z. (p(y) /\ r(z) /\ ~q(y, z, w))
        let mut used_vars = HashSet::from([Var::new("w"), Var::new("y"), Var::new("z")]);
        assert_eq!(result_formula, to_pnf(formula, &mut used_vars));
    }

    #[test]
    fn test_to_pnf_2() {
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
        let result_formula = Forall!(
            "w",
            Exists!(
                "z0",
                Forall!(
                    "x",
                    Forall!(
                        "y",
                        Exists!(
                            "z",
                            Or!(
                                And!(
                                    Pred!("p", [Var!("x"), Var!("z")]),
                                    Neg!(Pred!("q", [Var!("y"), Var!("z")]))
                                ),
                                Pred!("p", [Var!("w"), Var!("z0")])
                            )
                        )
                    )
                )
            )
        ); //forall w. exists z0. forall x. forall y. exists z. ((p(x, z) /\ ~q(y, z)) \/ p(w, z0))
        let mut used_vars = HashSet::from([
            Var::new("w"),
            Var::new("x"),
            Var::new("y"),
            Var::new("z"),
        ]);
        assert_eq!(result_formula, to_pnf(formula, &mut used_vars));
    }
}
