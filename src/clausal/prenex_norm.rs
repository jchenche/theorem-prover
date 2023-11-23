use crate::{lang::Formula, And, Exists, Iff, Imply, Neg, Or};

pub fn to_pnf(formula: Formula) -> Formula {
    let nnf = to_nnf(formula);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Neg, Obj, Or, Pred, Var,
    };

    #[test]
    fn test_to_pnf_simple() {
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
        assert_eq!(to_pnf(formula), expected_result);
    }

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
}
