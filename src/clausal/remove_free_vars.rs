use crate::lang::{Formula, Var, Term};

use super::Environment;

pub fn remove_free_vars(formula: Formula) -> Formula {
    let mut free_vars = vec![];
    let mut env = Environment::new();
    find_free_vars(&formula, &mut env, &mut free_vars);
    free_vars.iter().rfold(formula, |acc, free_var| {
        Formula::Exists(free_var.clone(), Box::new(acc))
    })
}

fn find_free_vars(formula: &Formula, env: &mut Environment, free_vars: &mut Vec<Var>) {
    match formula {
        Formula::Pred(pred) => pred
            .get_args()
            .iter()
            .for_each(|arg| find_free_vars_in_terms(arg, env, free_vars)),
        Formula::True => {}
        Formula::False => {}
        Formula::And(left, right) => {
            find_free_vars(left, env, free_vars);
            find_free_vars(right, env, free_vars);
        }
        Formula::Or(left, right) => {
            find_free_vars(left, env, free_vars);
            find_free_vars(right, env, free_vars);
        }
        Formula::Neg(subformula) => {
            find_free_vars(subformula, env, free_vars);
        }
        Formula::Imply(left, right) => {
            find_free_vars(left, env, free_vars);
            find_free_vars(right, env, free_vars);
        }
        Formula::Iff(left, right) => {
            find_free_vars(left, env, free_vars);
            find_free_vars(right, env, free_vars);
        }
        Formula::Forall(var, subformula) => {
            env.push_scope();
            env.add(var.clone());
            find_free_vars(subformula, env, free_vars);
            env.pop_scope();
        }
        Formula::Exists(var, subformula) => {
            env.push_scope();
            env.add(var.clone());
            find_free_vars(subformula, env, free_vars);
            env.pop_scope();
        }
    }
}

fn find_free_vars_in_terms(term: &Term, env: &mut Environment, free_vars: &mut Vec<Var>) {
    match term {
        Term::Obj(_) => {}
        Term::Var(var) => {
            if let None = env.find(var) {
                free_vars.push(var.clone());
            }
        }
        Term::Fun(f) => f
            .get_args()
            .iter()
            .for_each(|arg| find_free_vars_in_terms(arg, env, free_vars)),
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
    fn test_remove_free_vars_simple() {
        let formula = Forall!(
            "x",
            And!(
                Pred!("p", [Var!("x"), Var!("z")]),
                Exists!("y", Pred!("q", [Var!("y"), Var!("w")]))
            )
        );
        let expected_result = Exists!(
            "z",
            Exists!(
                "w",
                Forall!(
                    "x",
                    And!(
                        Pred!("p", [Var!("x"), Var!("z")]),
                        Exists!("y", Pred!("q", [Var!("y"), Var!("w")]))
                    )
                )
            )
        );
        assert_eq!(remove_free_vars(formula), expected_result);
    }
}
