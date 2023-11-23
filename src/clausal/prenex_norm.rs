use crate::lang::{Formula, Term, Var};

use super::Environment;

pub fn to_pnf(formula: Formula) -> Formula {
    let nnf = to_nnf(formula);
    todo!()
}

fn to_nnf(formula: Formula) -> Formula {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Fun, Obj, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Neg, Obj, Pred, Var, Or
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
                Exists!(
                    "y",
                    Pred!("p", [Var!("x"), Var!("y")])
                )
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
}
