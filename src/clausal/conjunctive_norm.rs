use crate::{lang::Formula, And, Neg, Or};

pub fn to_cnf(formula: Formula) -> Formula {
    let nnf = super::prenex_norm::to_nnf(formula);
    let cnf = dist_or_over_and(nnf);
    return cnf;
}

fn dist_or_over_and(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(left, right) => And!(dist_or_over_and(*left), dist_or_over_and(*right)),
        Formula::Or(left, right) => match (dist_or_over_and(*left), dist_or_over_and(*right)) {
            (Formula::And(subleft, subright), right) => And!(
                dist_or_over_and(Or!(*subleft, right.clone())),
                dist_or_over_and(Or!(*subright, right))
            ),
            (left, Formula::And(subleft, subright)) => And!(
                dist_or_over_and(Or!(left.clone(), *subleft)),
                dist_or_over_and(Or!(left, *subright))
            ),
            (left, right) => Or!(left, right),
        },
        Formula::Neg(subformula) => Neg!(dist_or_over_and(*subformula)),
        Formula::Imply(_, _) => unreachable!("After conversion to NNF, there shouldn't be ->"),
        Formula::Iff(_, _) => unreachable!("After conversion to NNF, there shouldn't be <->"),
        Formula::Forall(var, subformula) => {
            Formula::Forall(var, Box::new(dist_or_over_and(*subformula)))
        }
        Formula::Exists(var, subformula) => {
            Formula::Exists(var, Box::new(dist_or_over_and(*subformula)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Forall, Fun, Neg, Obj, Or, Pred, Var,
    };

    #[test]
    fn test_to_cnf_1() {
        let formula = Forall!(
            "x",
            Forall!(
                "w",
                Forall!(
                    "y",
                    Forall!(
                        "z",
                        Or!(
                            Pred!("p", [Var!("x")]),
                            And!(
                                And!(Pred!("p", [Var!("w")]), Pred!("p", [Var!("y")])),
                                Pred!("p", [Var!("z")])
                            )
                        )
                    )
                )
            )
        ); // forall x. forall w. forall y. forall z. (p(x) \/ (p(w) /\ p(y) /\ p(z)))
        let result_formula = Forall!(
            "x",
            Forall!(
                "w",
                Forall!(
                    "y",
                    Forall!(
                        "z",
                        And!(
                            And!(
                                Or!(Pred!("p", [Var!("x")]), Pred!("p", [Var!("w")])),
                                Or!(Pred!("p", [Var!("x")]), Pred!("p", [Var!("y")]))
                            ),
                            Or!(Pred!("p", [Var!("x")]), Pred!("p", [Var!("z")]))
                        )
                    )
                )
            )
        ); // forall x. forall w. forall y. forall z. ((p(x) \/ p(w)) /\ (p(x) \/ p(y)) /\ (p(x) \/ p(z))
        assert_eq!(to_cnf(formula), result_formula);
    }

    #[test]
    fn test_to_cnf_2() {
        let formula = Forall!(
            "x",
            Or!(
                And!(Neg!(Pred!("p", [Var!("x")])), Pred!("q", [Var!("x")])),
                And!(Neg!(Pred!("r", [Var!("x")])), Pred!("r", [Var!("x")]))
            )
        ); // forall x. ((~p(x) /\ q(x)) \/ (~r(x) /\ r(x)))
        let result_formula = Forall!(
            "x",
            And!(
                And!(
                    And!(
                        Or!(Neg!(Pred!("p", [Var!("x")])), Neg!(Pred!("r", [Var!("x")]))),
                        Or!(Neg!(Pred!("p", [Var!("x")])), Pred!("r", [Var!("x")]))
                    ),
                    Or!(Pred!("q", [Var!("x")]), Neg!(Pred!("r", [Var!("x")])))
                ),
                Or!(Pred!("q", [Var!("x")]), Pred!("r", [Var!("x")]))
            )
        ); // forall x. ((~p(x) \/ ~r(x)) /\ (~p(x) \/ r(x)) /\ (q(x) \/~r(x)) /\ (q(x) \/ r(x)))
        assert_eq!(to_cnf(formula), result_formula);
    }

    #[test]
    fn test_to_cnf_3() {
        let formula = Forall!(
            "z",
            Forall!(
                "x",
                And!(
                    Or!(
                        Neg!(Pred!("p", [Var!("z"), Obj!("a")])),
                        Or!(
                            Neg!(Pred!("p", [Var!("z"), Var!("x")])),
                            Neg!(Pred!("p", [Var!("x"), Var!("z")]))
                        )
                    ),
                    Or!(
                        Pred!("p", [Var!("z"), Obj!("a")]),
                        And!(
                            Pred!("p", [Var!("z"), Fun!("f", [Var!("z")])]),
                            Pred!("p", [Fun!("f", [Var!("z")]), Var!("z")])
                        )
                    )
                )
            )
        );
        // forall z. forall x.((
        //     (~p(z, a) \/ (~p(z, x) \/ ~p(x, z)))
        //     /\ (p(z, a) \/ (p(z, f(z)), p(f(z), z)))
        // ))

        let result_formula = Forall!(
            "z",
            Forall!(
                "x",
                And!(
                    And!(
                        Or!(
                            Or!(
                                Neg!(Pred!("p", [Var!("z"), Obj!("a")])),
                                Neg!(Pred!("p", [Var!("z"), Var!("x")]))
                            ),
                            Neg!(Pred!("p", [Var!("x"), Var!("z")]))
                        ),
                        Or!(
                            Pred!("p", [Var!("z"), Obj!("a")]),
                            Pred!("p", [Var!("z"), Fun!("f", [Var!("z")])])
                        )
                    ),
                    Or!(
                        Pred!("p", [Var!("z"), Obj!("a")]),
                        Pred!("p", [Fun!("f", [Var!("z")]), Var!("z")])
                    )
                )
            )
        );
        // forall z.(forall x.(
        //     (~p(z, a) \/ ~p(z, x) \/ ~p(x, z))
        //     /\ (p(z, a) \/ p(z, f(z)))
        //     /\ (p(z, a) \/ p(f(z), z))
        // ))

        assert_eq!(to_cnf(formula), result_formula);
    }
}
