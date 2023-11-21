use lang::Formula;

mod clausal;
pub mod lang;
mod resolution;
mod unification;

pub fn is_valid(formula: Formula, limit_in_seconds: u64) -> Option<bool> {
    let negated = Formula::Neg(Box::new(formula));
    let clausal = clausal::to_clausal(negated);
    resolution::refute_resolution(clausal, limit_in_seconds)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Pred, Term, Var},
        Forall, Neg, Pred, Var,
    };

    const DEFAULT_LIMIT: u64 = 60;

    #[test]
    fn test_valid_formula() {
        // F : ~(exists y.(forall z.((p(z, y)) <-> (~(exists x.((p(z, x)) /\ (p(x, z))))))))
        let formula = Neg!(Exists!(
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
        ));

        assert!(is_valid(formula, DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_invalid_formula() {
        // F : exists x.(p(x)) -> forall x.(p(x))
        let formula = Imply!(
            Exists!("x", Pred!("p", [Var!("x")])),
            Forall!("x", Pred!("p", [Var!("x")]))
        );

        assert!(!is_valid(formula, DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_unknown_formula() {
        // F: forall x.(forall y.((p(x) \/ ~q(x)) /\ (~p(y) /\ q(y))))
        let formula = Forall!(
            "x",
            Forall!(
                "y",
                And!(
                    Or!(Pred!("p", [Var!("x")]), Neg!(Pred!("q", [Var!("x")]))),
                    Or!(Neg!(Pred!("p", [Var!("y")])), Pred!("q", [Var!("y")]))
                )
            )
        );

        assert!(is_valid(formula, DEFAULT_LIMIT).is_none());
    }
}
