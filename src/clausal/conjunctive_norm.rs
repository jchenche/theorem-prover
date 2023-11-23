use crate::lang::{Formula, Term, Var};

use super::Environment;

pub fn to_cnf(formula: Formula) -> Formula {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Neg, Obj, Or, Pred, Var,
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

        assert_eq!(result_formula, to_cnf(formula));
    }
}
