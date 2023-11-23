use std::collections::HashSet;

use crate::{
    lang::{Formula, Fun, Obj, Pred, Term, Var},
    And, Iff, Imply, Neg, Or,
};

use super::Environment;

pub fn skolemize(formula: Formula) -> Formula {
    let mut used_funcs = HashSet::new();
    let mut used_objs = HashSet::new();
    gather_funcs_and_objs(formula.clone(), &mut used_funcs, &mut used_objs);
    let mut seen_bound_vars = vec![];
    let mut env = Environment::new();
    let no_exists = remove_existentials(
        formula,
        &mut seen_bound_vars,
        &mut env,
        &mut used_funcs,
        &mut used_objs,
    );
    return no_exists;
}

fn gather_funcs_and_objs(
    formula: Formula,
    used_funcs: &mut HashSet<String>,
    used_objs: &mut HashSet<String>,
) {
    match formula {
        Formula::Pred(pred) => pred
            .get_args()
            .iter()
            .for_each(|arg| gather_funcs_and_objs_in_terms(arg, used_funcs, used_objs)),
        Formula::True => {}
        Formula::False => {}
        Formula::And(left, right) => {
            gather_funcs_and_objs(*left, used_funcs, used_objs);
            gather_funcs_and_objs(*right, used_funcs, used_objs);
        }
        Formula::Or(left, right) => {
            gather_funcs_and_objs(*left, used_funcs, used_objs);
            gather_funcs_and_objs(*right, used_funcs, used_objs);
        }
        Formula::Neg(subformula) => {
            gather_funcs_and_objs(*subformula, used_funcs, used_objs);
        }
        Formula::Imply(left, right) => {
            gather_funcs_and_objs(*left, used_funcs, used_objs);
            gather_funcs_and_objs(*right, used_funcs, used_objs);
        }
        Formula::Iff(left, right) => {
            gather_funcs_and_objs(*left, used_funcs, used_objs);
            gather_funcs_and_objs(*right, used_funcs, used_objs);
        }
        Formula::Forall(_, subformula) => {
            gather_funcs_and_objs(*subformula, used_funcs, used_objs);
        }
        Formula::Exists(_, subformula) => {
            gather_funcs_and_objs(*subformula, used_funcs, used_objs);
        }
    }
}

fn gather_funcs_and_objs_in_terms(
    term: &Term,
    used_funcs: &mut HashSet<String>,
    used_objs: &mut HashSet<String>,
) {
    match term {
        Term::Obj(obj) => {
            used_objs.insert(format!("{}", obj));
        }
        Term::Var(_) => {}
        Term::Fun(f) => {
            used_funcs.insert(f.get_id().clone());
            f.get_args()
                .iter()
                .for_each(|arg| gather_funcs_and_objs_in_terms(arg, used_funcs, used_objs))
        }
    }
}

fn remove_existentials(
    formula: Formula,
    seen_bound_vars: &mut Vec<Var>,
    env: &mut Environment,
    used_funcs: &mut HashSet<String>,
    used_objs: &mut HashSet<String>,
) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(Pred::new(
            pred.get_id(),
            Box::new(
                pred.get_args()
                    .iter()
                    .map(|arg| remove_existentials_in_terms(arg, env))
                    .collect(),
            ),
        )),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(
            remove_existentials(*left, seen_bound_vars, env, used_funcs, used_objs),
            remove_existentials(*right, seen_bound_vars, env, used_funcs, used_objs)
        ),
        Formula::Or(left, right) => Or!(
            remove_existentials(*left, seen_bound_vars, env, used_funcs, used_objs),
            remove_existentials(*right, seen_bound_vars, env, used_funcs, used_objs)
        ),
        Formula::Neg(subformula) => Neg!(remove_existentials(
            *subformula,
            seen_bound_vars,
            env,
            used_funcs,
            used_objs
        )),
        Formula::Imply(left, right) => Imply!(
            remove_existentials(*left, seen_bound_vars, env, used_funcs, used_objs),
            remove_existentials(*right, seen_bound_vars, env, used_funcs, used_objs)
        ),
        Formula::Iff(left, right) => Iff!(
            remove_existentials(*left, seen_bound_vars, env, used_funcs, used_objs),
            remove_existentials(*right, seen_bound_vars, env, used_funcs, used_objs)
        ),
        Formula::Forall(var, subformula) => {
            seen_bound_vars.push(var.clone());
            Formula::Forall(
                var,
                Box::new(remove_existentials(
                    *subformula,
                    seen_bound_vars,
                    env,
                    used_funcs,
                    used_objs,
                )),
            )
        }
        Formula::Exists(var, subformula) => {
            let new_term = find_new_term(seen_bound_vars, used_funcs, used_objs);
            env.push_scope();
            env.add(var, new_term);
            remove_existentials(*subformula, seen_bound_vars, env, used_funcs, used_objs)
        }
    }
}

fn find_new_term(
    seen_bound_vars: &mut Vec<Var>,
    used_funcs: &mut HashSet<String>,
    used_objs: &mut HashSet<String>,
) -> Term {
    let mut suffix = 0;
    loop {
        if seen_bound_vars.is_empty() {
            let new_obj = format!("o{}", suffix.to_string());
            if !used_objs.contains(&new_obj) {
                used_objs.insert(new_obj.clone());
                return Term::Obj(Obj::new(&new_obj));
            }
        } else {
            let new_fun = format!("f{}", suffix.to_string());
            if !used_funcs.contains(&new_fun) {
                used_funcs.insert(new_fun.clone());
                return Term::Fun(Fun::new(
                    &new_fun,
                    Box::new(
                        seen_bound_vars
                            .iter()
                            .map(|var| Term::Var(var.clone()))
                            .collect(),
                    ),
                ));
            }
        }
        suffix += 1;
    }
}

fn remove_existentials_in_terms(term: &Term, env: &mut Environment) -> Term {
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
                    .map(|arg| remove_existentials_in_terms(arg, env))
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
        And, Exists, Forall, Fun, Neg, Obj, Or, Pred, Var,
    };

    #[test]
    fn test_skolemize_1() {
        let formula = Exists!(
            "w",
            Forall!(
                "y",
                Exists!(
                    "z",
                    And!(
                        And!(
                            Pred!("p", [Fun!("f", [Obj!("a")])]),
                            Pred!("r", [Var!("z")])
                        ),
                        Neg!(Pred!("q", [Var!("y"), Var!("z"), Var!("w")]))
                    )
                )
            )
        ); // exists w. forall y. exists z. (p(f(a)) /\ r(z) /\ ~q(y, z, w))
        let result_formula = Forall!(
            "y",
            And!(
                And!(
                    Pred!("p", [Fun!("f", [Obj!("a")])]),
                    Pred!("r", [Fun!("f0", [Var!("y")])])
                ),
                Neg!(Pred!("q", [Var!("y"), Fun!("f0", [Var!("y")]), Obj!("o0")]))
            )
        ); // forall y. (p(f(a)) /\ r(f0(y)) /\ ~q(y, f0(y), o0))
        assert_eq!(skolemize(formula), result_formula);
    }

    #[test]
    fn test_skolemize_2() {
        let formula = Forall!(
            "w",
            Exists!(
                "u",
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
                                Pred!("p", [Var!("w"), Var!("u")])
                            )
                        )
                    )
                )
            )
        ); // forall w. exists z0. forall x. forall y. exists z. ((p(x, z) /\ ~q(y, z)) \/ p(w, z0))
        let result_formula = Forall!(
            "w",
            Forall!(
                "x",
                Forall!(
                    "y",
                    Or!(
                        And!(
                            Pred!(
                                "p",
                                [Var!("x"), Fun!("f1", [Var!("w"), Var!("x"), Var!("y")])]
                            ),
                            Neg!(Pred!(
                                "q",
                                [Var!("y"), Fun!("f1", [Var!("w"), Var!("x"), Var!("y")])]
                            ))
                        ),
                        Pred!("p", [Var!("w"), Fun!("f0", [Var!["w"]])])
                    )
                )
            )
        ); // forall w. forall x. forall y. ((p(x, f1(w, x, y)) /\ ~q(y, f1(w, x, y))) \/ p(w, f0(w)))
        assert_eq!(skolemize(formula), result_formula);
    }
}
