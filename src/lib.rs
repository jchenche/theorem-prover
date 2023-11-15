pub mod lang;
mod clausal;
mod unification;
mod resolution;

use lang::Formula;

#[macro_export]
macro_rules! boxed {
    ($value:expr) => {
        Box::new($value)
    };
}

// Example of how to transform the formula (not the most efficient but it's the most readable)
// These will mostly be defined in clausal.rs
fn and_to_or(formula: Formula) -> Formula {
    match formula {
        Formula::Pred(pred) => Formula::Pred(pred),
        Formula::True => Formula::True,
        Formula::False => Formula::False,
        Formula::And(l, r) => Formula::Or(boxed!(and_to_or(*l)), boxed!(and_to_or(*r))),
        Formula::Or(l, r) => Formula::Or(boxed!(and_to_or(*l)), boxed!(and_to_or(*r))),
        Formula::Neg(l) => Formula::Neg(boxed!(and_to_or(*l))),
        Formula::Imply(l, r) => Formula::Imply(boxed!(and_to_or(*l)), boxed!(and_to_or(*r))),
        Formula::Iff(l, r) => Formula::Iff(boxed!(and_to_or(*l)), boxed!(and_to_or(*r))),
        Formula::Forall(v, l) => Formula::Forall(v, boxed!(and_to_or(*l))),
        Formula::Exists(v, l) => Formula::Exists(v, boxed!(and_to_or(*l))),
    }
}

pub fn is_valid(formula: Formula) -> bool {
    // let example_conversion = and_to_or(formula);
    // println!("{}", example_conversion);
    let negated = Formula::Neg(boxed!(formula));
    let clausal = clausal::to_clausal(negated);
    resolution::resolve(clausal)
}
