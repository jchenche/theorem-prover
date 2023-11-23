use std::collections::HashSet;

use crate::{
    lang::{Formula, Var},
    And, Exists, Iff, Imply, Neg, Or,
};

use super::Environment;

pub fn to_pnf(formula: Formula, used_vars: HashSet<Var>) -> Formula {
    let nnf = to_nnf(formula);
    let seen_vars = HashSet::new();
    let env = Environment::new();
    let bound_vars_renamed = rename_bound_vars(nnf, env, seen_vars, used_vars);
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
    env: Environment,
    seen_var: HashSet<Var>,
    used_vars: HashSet<Var>,
) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => todo!(),
        Formula::Or(left, right) => todo!(),
        Formula::Neg(subformula) => todo!(),
        Formula::Imply(left, right) => todo!(),
        Formula::Iff(left, right) => todo!(),
        Formula::Forall(var, subformula) => todo!(),
        Formula::Exists(var, subformula) => todo!(),
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
        let used_vars = HashSet::from([Var::new("x"), Var::new("y"), Var::new("z")]);
        assert_eq!(to_pnf(formula, used_vars), expected_result);
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
        let used_vars = HashSet::from([Var::new("w"), Var::new("y"), Var::new("z")]);
        assert_eq!(result_formula, to_pnf(formula, used_vars));
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
        let used_vars = HashSet::from([
            Var::new("w"),
            Var::new("z0"),
            Var::new("x"),
            Var::new("y"),
            Var::new("z"),
        ]);
        assert_eq!(result_formula, to_pnf(formula, used_vars));
    }
}
