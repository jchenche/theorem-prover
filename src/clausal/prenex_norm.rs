use std::collections::HashSet;

use crate::{
    lang::{Formula, Fun, Pred, Term, Var},
    And, Iff, Imply, Neg, Or,
};

use super::Environment;

enum Quantifier {
    AForall,
    AnExists,
}

pub fn to_pnf(formula: Formula) -> Formula {
    let mut used_vars = super::get_used_bound_vars(formula.clone());
    let nnf = to_nnf(formula);
    let mut seen_vars = HashSet::new();
    let mut env = Environment::new();
    let bound_vars_renamed = rename_bound_vars(nnf, &mut env, &mut seen_vars, &mut used_vars);
    let pnf = move_quantifiers_to_front(bound_vars_renamed);
    return pnf;
}

pub fn to_nnf(formula: Formula) -> Formula {
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
    seen_vars: &mut HashSet<Var>,
    used_vars: &mut HashSet<String>,
) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(Pred::new(
            pred.get_id(),
            Box::new(
                pred.get_args()
                    .iter()
                    .map(|arg| rename_bound_vars_in_terms(arg, env, seen_vars, used_vars))
                    .collect(),
            ),
        )),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(
            rename_bound_vars(*left, env, seen_vars, used_vars),
            rename_bound_vars(*right, env, seen_vars, used_vars)
        ),
        Formula::Or(left, right) => Or!(
            rename_bound_vars(*left, env, seen_vars, used_vars),
            rename_bound_vars(*right, env, seen_vars, used_vars)
        ),
        Formula::Neg(subformula) => Neg!(rename_bound_vars(*subformula, env, seen_vars, used_vars)),
        Formula::Imply(left, right) => Imply!(
            rename_bound_vars(*left, env, seen_vars, used_vars),
            rename_bound_vars(*right, env, seen_vars, used_vars)
        ),
        Formula::Iff(left, right) => Iff!(
            rename_bound_vars(*left, env, seen_vars, used_vars),
            rename_bound_vars(*right, env, seen_vars, used_vars)
        ),
        Formula::Forall(var, subformula) => {
            if seen_vars.contains(&var) {
                let new_var = find_new_var(&var, used_vars);
                seen_vars.insert(new_var.clone());
                env.push_scope();
                env.add(var.clone(), Term::Var(new_var.clone()));
                let subformula = rename_bound_vars(*subformula, env, seen_vars, used_vars);
                env.pop_scope();
                seen_vars.insert(var);
                Formula::Forall(new_var, Box::new(subformula))
            } else {
                let subformula = rename_bound_vars(*subformula, env, seen_vars, used_vars);
                seen_vars.insert(var.clone());
                Formula::Forall(var, Box::new(subformula))
            }
        }
        Formula::Exists(var, subformula) => {
            if seen_vars.contains(&var) {
                let new_var = find_new_var(&var, used_vars);
                seen_vars.insert(new_var.clone());
                env.push_scope();
                env.add(var.clone(), Term::Var(new_var.clone()));
                let subformula = rename_bound_vars(*subformula, env, seen_vars, used_vars);
                env.pop_scope();
                seen_vars.insert(var);
                Formula::Exists(new_var, Box::new(subformula))
            } else {
                let subformula = rename_bound_vars(*subformula, env, seen_vars, used_vars);
                seen_vars.insert(var.clone());
                Formula::Exists(var, Box::new(subformula))
            }
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

fn rename_bound_vars_in_terms(
    term: &Term,
    env: &mut Environment,
    seen_vars: &mut HashSet<Var>,
    used_vars: &mut HashSet<String>,
) -> Term {
    match term {
        Term::Obj(_) => term.clone(),
        Term::Var(v) => {
            if let Some(new_term) = env.find(v) {
                new_term
            } else {
                term.clone()
            }
        }
        Term::Fun(f) => Term::Fun(Fun::new(
            f.get_id(),
            Box::new(
                f.get_args()
                    .iter()
                    .map(|arg| rename_bound_vars_in_terms(arg, env, seen_vars, used_vars))
                    .collect(),
            ),
        )),
    }
}

fn move_quantifiers_to_front(formula: Formula) -> Formula {
    let inner_formula = find_inner_formula(formula.clone());
    let mut inner_quantifiers = vec![];
    let inner_formula_no_quantifiers = find_quantifiers(inner_formula, &mut inner_quantifiers);
    let all_quantifiers_front =
        insert_inner_quantifiers(formula, inner_quantifiers, inner_formula_no_quantifiers);
    return all_quantifiers_front;
}

fn find_inner_formula(formula: Formula) -> Formula {
    match formula {
        Formula::Forall(_, subformula) | Formula::Exists(_, subformula) => match *subformula {
            Formula::Forall(_, _) | Formula::Exists(_, _) => find_inner_formula(*subformula),
            _ => *subformula,
        },
        _ => formula,
    }
}

fn find_quantifiers(formula: Formula, inner_quantifiers: &mut Vec<(Var, Quantifier)>) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(
            find_quantifiers(*left, inner_quantifiers),
            find_quantifiers(*right, inner_quantifiers)
        ),
        Formula::Or(left, right) => Or!(
            find_quantifiers(*left, inner_quantifiers),
            find_quantifiers(*right, inner_quantifiers)
        ),
        Formula::Neg(subformula) => Neg!(find_quantifiers(*subformula, inner_quantifiers)),
        Formula::Imply(left, right) => Imply!(
            find_quantifiers(*left, inner_quantifiers),
            find_quantifiers(*right, inner_quantifiers)
        ),
        Formula::Iff(left, right) => Iff!(
            find_quantifiers(*left, inner_quantifiers),
            find_quantifiers(*right, inner_quantifiers)
        ),
        Formula::Forall(var, subformula) => {
            inner_quantifiers.push((var, Quantifier::AForall));
            find_quantifiers(*subformula, inner_quantifiers)
        }
        Formula::Exists(var, subformula) => {
            inner_quantifiers.push((var, Quantifier::AnExists));
            find_quantifiers(*subformula, inner_quantifiers)
        }
    }
}

fn insert_inner_quantifiers(
    formula: Formula,
    inner_quantifiers: Vec<(Var, Quantifier)>,
    inner_formula_no_quantifiers: Formula,
) -> Formula {
    match formula {
        Formula::Forall(var, subformula) => Formula::Forall(
            var,
            Box::new(match *subformula {
                Formula::Forall(_, _) | Formula::Exists(_, _) => insert_inner_quantifiers(
                    *subformula,
                    inner_quantifiers,
                    inner_formula_no_quantifiers,
                ),
                _ => inner_quantifiers.into_iter().rfold(
                    inner_formula_no_quantifiers,
                    |acc, inner_quantifier| match inner_quantifier.1 {
                        Quantifier::AForall => Formula::Forall(inner_quantifier.0, Box::new(acc)),
                        Quantifier::AnExists => Formula::Exists(inner_quantifier.0, Box::new(acc)),
                    },
                ),
            }),
        ),
        Formula::Exists(var, subformula) => Formula::Exists(
            var,
            Box::new(match *subformula {
                Formula::Forall(_, _) | Formula::Exists(_, _) => insert_inner_quantifiers(
                    *subformula,
                    inner_quantifiers,
                    inner_formula_no_quantifiers,
                ),
                _ => inner_quantifiers.into_iter().rfold(
                    inner_formula_no_quantifiers,
                    |acc, inner_quantifier| match inner_quantifier.1 {
                        Quantifier::AForall => Formula::Forall(inner_quantifier.0, Box::new(acc)),
                        Quantifier::AnExists => Formula::Exists(inner_quantifier.0, Box::new(acc)),
                    },
                ),
            }),
        ),
        _ => inner_quantifiers.into_iter().rfold(
            inner_formula_no_quantifiers,
            |acc, inner_quantifier| match inner_quantifier.1 {
                Quantifier::AForall => Formula::Forall(inner_quantifier.0, Box::new(acc)),
                Quantifier::AnExists => Formula::Exists(inner_quantifier.0, Box::new(acc)),
            },
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Obj, Pred, Term, Var},
        And, Exists, Forall, Neg, Obj, Or, Pred, Var,
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
    fn test_to_pnf_simple() {
        let formula = Imply!(
            Pred!("p", [Obj!("a")]),
            Forall!("x", Pred!("q", [Var!("x")]))
        ); // p(a) -> forall x. q(x)
        let expected_result = Forall!(
            "x",
            Or!(Neg!(Pred!("p", [Obj!("a")])), Pred!("q", [Var!("x")]))
        ); // forall x. (~p(a) \/ q(x))
        assert_eq!(to_pnf(formula), expected_result);
    }

    #[test]
    fn test_to_pnf_complex() {
        let formula = Forall!(
            "x",
            Or!(
                Neg!(Exists!(
                    "y",
                    And!(
                        Pred!("p", [Var!("x"), Var!("y")]),
                        Pred!("p", [Var!("x"), Obj!("a")])
                    )
                )),
                Exists!("y", Pred!("p", [Var!("x"), Var!("y")]))
            )
        ); // forall x. (~(exists y. (p(x, y) /\ p(x, a))) \/ exists y. p(x, y))
        let expected_result = Forall!(
            "x",
            Forall!(
                "y",
                Exists!(
                    "y0",
                    Or!(
                        Or!(
                            Neg!(Pred!("p", [Var!("x"), Var!("y")])),
                            Neg!(Pred!("p", [Var!("x"), Obj!("a")]))
                        ),
                        Pred!("p", [Var!("x"), Var!("y0")])
                    )
                )
            )
        ); // forall x. (forall y. (exists y0. ((~p(x, y) \/ ~p(x, a)) \/ p(x,y0))))
        assert_eq!(to_pnf(formula), expected_result);
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
        ); // exists w. forall y. (p(y) /\ ~(forall z. (r(z) -> q(y, z, w))))
        let result_formula = Exists!(
            "w",
            Forall!(
                "y",
                Exists!(
                    "z",
                    And!(
                        Pred!("p", [Var!("y")]),
                        And!(
                            Pred!("r", [Var!("z")]),
                            Neg!(Pred!("q", [Var!("y"), Var!("z"), Var!("w")]))
                        )
                    )
                )
            )
        ); // exists w. forall y. exists z. (p(y) /\ (r(z) /\ ~q(y, z, w)))
        assert_eq!(to_pnf(formula), result_formula);
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
            Forall!(
                "x",
                Forall!(
                    "y",
                    Exists!(
                        "z",
                        Exists!(
                            "z0",
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
        ); // forall w. forall x. forall y. exists z. exists z0. ((p(x, z) /\ ~q(y, z)) \/ p(w, z0))
        assert_eq!(to_pnf(formula), result_formula);
    }
}
