use lang::{Formula, Term};
use log::trace;
use std::collections::HashMap;

mod clausal;
pub mod lang;
mod resolution;
mod unification;

type Arity = HashMap<String, usize>;

#[derive(Debug)]
pub enum ProverError {
    ArityError,
    OtherError(String),
}

pub fn is_valid(formula: Formula, limit_in_seconds: u64) -> Result<Option<bool>, ProverError> {
    if !check_arity(&formula) {
        return Err(ProverError::ArityError);
    }
    trace!("Negating {formula}");
    let negated = Formula::Neg(Box::new(formula));
    let clausal = clausal::to_clausal(negated);
    Ok(resolution::refute_resolution(clausal, limit_in_seconds))
}

fn check_arity(formula: &Formula) -> bool {
    let mut pred_arity = HashMap::new();
    let mut func_arity = HashMap::new();
    check_arity_in_formula(formula, &mut pred_arity, &mut func_arity)
}

fn check_arity_in_formula(
    formula: &Formula,
    pred_arity: &mut Arity,
    func_arity: &mut Arity,
) -> bool {
    match formula {
        Formula::Pred(pred) => {
            if let Some(arity) = pred_arity.get(pred.get_id()) {
                if arity != &pred.get_args().len() {
                    return false;
                }
            } else {
                pred_arity.insert(pred.get_id().to_string(), pred.get_args().len());
            }
            pred.get_args()
                .iter()
                .all(|arg| check_arity_in_term(arg, pred_arity, func_arity))
        }
        Formula::True => true,
        Formula::False => true,
        Formula::And(left, right) => {
            check_arity_in_formula(left, pred_arity, func_arity)
                && check_arity_in_formula(right, pred_arity, func_arity)
        }
        Formula::Or(left, right) => {
            check_arity_in_formula(left, pred_arity, func_arity)
                && check_arity_in_formula(right, pred_arity, func_arity)
        }
        Formula::Neg(subformula) => check_arity_in_formula(subformula, pred_arity, func_arity),
        Formula::Imply(left, right) => {
            check_arity_in_formula(left, pred_arity, func_arity)
                && check_arity_in_formula(right, pred_arity, func_arity)
        }
        Formula::Iff(left, right) => {
            check_arity_in_formula(left, pred_arity, func_arity)
                && check_arity_in_formula(right, pred_arity, func_arity)
        }
        Formula::Forall(_, subformula) => {
            check_arity_in_formula(subformula, pred_arity, func_arity)
        }
        Formula::Exists(_, subformula) => {
            check_arity_in_formula(subformula, pred_arity, func_arity)
        }
    }
}

fn check_arity_in_term(term: &Term, pred_arity: &mut Arity, func_arity: &mut Arity) -> bool {
    match term {
        Term::Obj(_) => true,
        Term::Var(_) => true,
        Term::Fun(f) => {
            if let Some(arity) = func_arity.get(f.get_id()) {
                if arity != &f.get_args().len() {
                    return false;
                }
            } else {
                func_arity.insert(f.get_id().to_string(), f.get_args().len());
            }
            f.get_args()
                .iter()
                .all(|arg| check_arity_in_term(arg, pred_arity, func_arity))
        }
    }
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
        assert!(is_valid(formula, DEFAULT_LIMIT).unwrap().unwrap());
    }

    #[test]
    fn test_invalid_formula() {
        let formula = Imply!(
            Exists!("x", Pred!("p", [Var!("x")])),
            Forall!("x", Pred!("p", [Var!("x")]))
        ); // F : exists x.(p(x)) -> forall x.(p(x))
        assert!(!is_valid(formula, DEFAULT_LIMIT).unwrap().unwrap());
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
        assert!(is_valid(formula, 5).unwrap().is_none());
    }
}
