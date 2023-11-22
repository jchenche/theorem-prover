use crate::lang::{Clause, Formula};

pub fn to_clausal(formula: Formula) -> Vec<Clause> {
    todo!()
}

fn remove_free_vars(formula: Formula) -> Formula {
    todo!()
}

fn to_pnf(formula: Formula) -> Formula {
    todo!()
}

fn skolemize(formula: Formula) -> Formula {
    todo!()
}

fn to_cnf(formula: Formula) -> Formula {
    todo!()
}

fn to_clausal_form(formula: Formula) -> Vec<Clause> {
    todo!()
}

// example transformation
fn and_to_or(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(l, r) => Formula::Or(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Or(l, r) => Formula::Or(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Neg(f) => Formula::Neg(Box::new(and_to_or(*f))),
        Formula::Imply(l, r) => Formula::Imply(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Iff(l, r) => Formula::Iff(Box::new(and_to_or(*l)), Box::new(and_to_or(*r))),
        Formula::Forall(v, f) => Formula::Forall(v, Box::new(and_to_or(*f))),
        Formula::Exists(v, f) => Formula::Exists(v, Box::new(and_to_or(*f))),
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
    fn test_to_clausal() {
        let formula = Neg!(Neg!(Exists!(
            "y",
            Forall!(
                "z",
                Iff!(
                    Pred!("p", [Var!("z"), Var!("y")]),
                    Neg!(Exists!(
                        "x",
                        And!(
                            Pred!("p", [Var!("z"), Var!("x")]),
                            Pred!("p", [Var!("x"), Var!("z")])
                        )
                    ))
                )
            )
        ))); // ~(~(exists y.(forall z.((p(z, y)) <-> (~(exists x.((p(z, x)) /\ (p(x, z)))))))))

        let p1 = Neg!(Pred!("p", [Var!("z"), Obj!("a")])); // ~p(z, a)
        let p2 = Neg!(Pred!("p", [Var!("z"), Var!("x")])); // ~p(z, x)
        let p3 = Neg!(Pred!("p", [Var!("x"), Var!("z")])); // ~p(x, z)
        let p4 = Pred!("p", [Var!("y"), Obj!("a")]); // p(y, a)
        let p5 = Pred!("p", [Var!("y"), Fun!("f", [Var!("y")])]); // p(y, f(y))
        let p6 = Pred!("p", [Var!("w"), Obj!("a")]); // p(w, a)
        let p7 = Pred!("p", [Fun!("f", [Var!("w")]), Var!("w")]); // p(f(w), w)

        let c1 = Clause::new(vec![p1, p2, p3]); // C1: {~p(z, a), ~p(z, x), ~p(x, z)}
        let c2 = Clause::new(vec![p4, p5]); // C2: {p(y, a), p(y, f(y))}
        let c3 = Clause::new(vec![p6, p7]); // C3: {p(y, a), p(f(w), w)}

        let expected_result = vec![c1, c2, c3];
        assert_eq!(to_clausal(formula), expected_result);
    }

    #[test]
    fn test_remove_free_vars_1() {
        let formula = Imply! (
            Pred! ("p", [Var! ("x")]),
            Imply! (
                Exists! (
                    "x", 
                    Imply! (
                        Pred! ("q", [Var! ("x")]),
                        Fun! ("f", [Var! ("x"), Var! ("y")])
                    )
                ),
                Pred! ("q", [Var! ("x")])
            )
        );
        let result_formula = Exists! (
            "x",
            Exists! (
                "y", 
                formula
            )
        );
        assert_eq!(result_formula, remove_free_vars(formula));
    }

    fn test_remove_free_vars_2() {
        let formula = Forall! (
            "y",
            And! (
                Pred! ("p", [Var! ["y"]]),
                Neg! (
                    Forall! (
                        "z",
                        Imply! (
                            Pred! ("r", [Var! ("z")]),
                            Pred! ("q", [Var! ("y"), Var! ("z"), Var! ("w")])
                        )
                    )
                )
            )
        );
        let result_formula = Exists! ("w", formula);
        assert_eq!(result_formula, remove_free_vars(formula));
    }
}
