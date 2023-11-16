use lang::Formula;

mod clausal;
pub mod lang;
mod resolution;
mod unification;

// Example of how to transform the formula (not the most efficient but it's the most readable)
// These will mostly be defined in clausal.rs
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

pub fn is_valid(formula: Formula) -> bool {
    // let example_conversion = and_to_or(formula);
    // println!("{}", example_conversion);
    let negated = Formula::Neg(Box::new(formula));
    let clausal = clausal::to_clausal(negated);
    resolution::resolve(clausal)
}
