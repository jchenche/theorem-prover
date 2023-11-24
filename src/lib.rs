use lang::Formula;
use log::trace;

mod clausal;
pub mod lang;
mod resolution;
mod unification;

pub fn is_valid(formula: Formula, limit_in_seconds: u64) -> Option<bool> {
    validate_formula(&formula);
    trace!("Negating {formula}");
    let negated = Formula::Neg(Box::new(formula));
    let clausal = clausal::to_clausal(negated);
    resolution::refute_resolution(clausal, limit_in_seconds)
}

fn validate_formula(formula: &Formula) {
    // To be implemented later (just panic if it's not right for now)
    assert!(true, "The formula is not right. Make sure that functions and predicates of the same symbol have the same signatures");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::{Pred, Term, Var};

    const DEFAULT_LIMIT: u64 = 60;

    #[test]
    fn test_valid_formula() {
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
        )); // F : ~(exists y.(forall z.((p(z, y)) <-> (~(exists x.((p(z, x)) /\ (p(x, z))))))))
        assert!(is_valid(formula, DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_invalid_formula() {
        let formula = Imply!(
            Exists!("x", Pred!("p", [Var!("x")])),
            Forall!("x", Pred!("p", [Var!("x")]))
        ); // F : exists x.(p(x)) -> forall x.(p(x))
        assert!(!is_valid(formula, DEFAULT_LIMIT).unwrap());
    }

    #[test]
    fn test_unknown_formula() {
        let formula = Neg!(Forall!(
            "x",
            Forall!(
                "y",
                And!(
                    Or!(Pred!("p", [Var!("x")]), Neg!(Pred!("q", [Var!("x")]))),
                    Or!(Neg!(Pred!("p", [Var!("y")])), Pred!("q", [Var!("y")]))
                )
            )
        )); // F: ~(forall x.(forall y.((p(x) \/ ~q(x)) /\ (~p(y) /\ q(y)))))
        assert!(is_valid(formula, 5).is_none());
    }
}
