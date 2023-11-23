use crate::lang::{Formula, Term, Var};

use super::Environment;

pub fn skolemize(formula: Formula) -> Formula {
    todo!()
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
                        And!(Pred!("p", [Var!("y")]), Pred!("r", [Var!("z")])),
                        Neg!(Pred!("q", [Var!("y"), Var!("z"), Var!("w")]))
                    )
                )
            )
        ); // exists w. forall y. exists z. (p(y) /\ r(z) /\ ~q(y, z, w))
        let result_formula = Forall!(
            "y",
            And!(
                And!(
                    Pred!("p", [Var!("y")]),
                    Pred!("r", [Fun!("f0", [Var!("y")])])
                ),
                Neg!(Pred!("q", [Var!("y"), Fun!("f0", [Var!("y")]), Obj!("o0")]))
            )
        ); // forall y. (p(y) /\ r(f0(y)) /\ ~q(y, f0(y), o0))
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
