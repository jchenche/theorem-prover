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
        assert_eq!(to_clausal_form(formula), result);
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
        assert_eq!(to_clausal_form(formula), result);
    }
}
