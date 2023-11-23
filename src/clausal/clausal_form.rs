use crate::lang::{Clause, Formula, Term, Var};

use super::Environment;

pub fn to_clausal_form(formula: Formula) -> Vec<Clause> {
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
    fn test_to_clausal_form_simple() {}

    #[test]
    fn test_to_clausal_form_1() {
        let formula = Forall!(
            "y",
            And!(
                And!(
                    Pred!("p", [Var!("y")]),
                    Pred!("r", [Fun!("f", [Var!("y")])])
                ),
                Neg!(Pred!("q", [Var!("y"), Fun!("f", [Var!("y")]), Obj!("c")]))
            )
        ); // forall y (p(y) /\ r(f(y)) /\ ~q(y, f(y), c)
        let p1 = Pred!("p", [Var!("y")]);
        let p2 = Pred!("r", [Fun!("f", [Var!("y0")])]);
        let p3 = Neg!(Pred!("q", [Var!("y1"), Fun!("f", [Var!("y1")]), Obj!("c")]));
        let c1 = Clause::new(vec![p1]);
        let c2 = Clause::new(vec![p2]);
        let c3 = Clause::new(vec![p3]);
        let result = vec![c1, c2, c3];
        assert_eq!(result, to_clausal_form(formula));
    }

    #[test]
    fn test_to_clausal_form_2() {
        let formula = Forall!(
            "w",
            Forall!(
                "x",
                Forall!(
                    "y",
                    And!(
                        Or!(
                            Pred!(
                                "p",
                                [Var!("x"), Fun!("g", [Var!("w"), Var!("x"), Var!("y")])]
                            ),
                            Pred!("p", [Var!("w"), Fun!("f", [Var!("w")])])
                        ),
                        Or!(
                            Neg!(Pred!(
                                "q",
                                [Var!("y"), Fun!("g", [Var!("w"), Var!("x"), Var!("y")])]
                            )),
                            Pred!("p", [Var!("w"), Fun!("f", [Var!("w")])])
                        )
                    )
                )
            )
        ); // forall w. forall. x. forall. y. ((p(x, g(w, x, y)) \/ p(w, f(w))) /\ (~q(y, g(w, x, y)) \/ p(w, f(w))))
        let p1 = Pred!(
            "p",
            [Var!("x"), Fun!("g", [Var!("w"), Var!("x"), Var!("y")])]
        );
        let p2 = Pred!("p", [Var!("w"), Fun!("f", [Var!("w")])]);
        let p3 = Neg!(Pred!(
            "q",
            [Var!("y0"), Fun!("g", [Var!("w0"), Var!("x0"), Var!("y0")])]
        ));
        let p4 = Pred!("p", [Var!("w0"), Fun!("f", [Var!("w0")])]);
        let c1 = Clause::new(vec![p1, p2]);
        let c2 = Clause::new(vec![p3, p4]);
        let result = vec![c1, c2];
        assert_eq!(result, to_clausal_form(formula));
    }

    #[test]
    fn test_to_clausal_form_3() {
        let formula = Forall!(
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

        let p1 = Neg!(Pred!("p", [Var!("z"), Obj!("a")]));
        let p2 = Neg!(Pred!("p", [Var!("z"), Var!("x")]));
        let p3 = Neg!(Pred!("p", [Var!("x"), Var!("z")]));
        let p4 = Pred!("p", [Var!("z0"), Obj!("a")]);
        let p5 = Pred!("p", [Var!("z0"), Fun!("f", [Var!("z0")])]);
        let p6 = Pred!("p", [Var!("z1"), Obj!("a")]);
        let p7 = Pred!("p", [Fun!("f", [Var!("z1")]), Var!("z1")]);

        let c1 = Clause::new(vec![p1, p2, p3]);
        let c2 = Clause::new(vec![p4, p5]);
        let c3 = Clause::new(vec![p6, p7]);

        let result = vec![c1, c2, c3];
        assert_eq!(result, to_clausal_form(formula));
    }
}
