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
    fn test_basic_formula_1() {
        // F : ¬( ∃y . ∀z . (p(z, y) ↔ ¬ ∃x . (p(z, x) ∧ p(x, z))))
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
}
