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
fn and_to_or(formula: Box<Formula>) -> Box<Formula> {
    match *formula {
        Formula::Pred(pred) => boxed!(Formula::Pred(pred)),
        Formula::True => boxed!(Formula::True),
        Formula::False => boxed!(Formula::False),
        Formula::And(l, r) => boxed!(Formula::Or(and_to_or(l), and_to_or(r))),
        Formula::Or(l, r) => boxed!(Formula::Or(and_to_or(l), and_to_or(r))),
        Formula::Neg(l) => boxed!(Formula::Neg(and_to_or(l))),
        Formula::Imply(l, r) => boxed!(Formula::Imply(and_to_or(l), and_to_or(r))),
        Formula::Iff(l, r) => boxed!(Formula::Iff(and_to_or(l), and_to_or(r))),
        Formula::Forall(v, l) => boxed!(Formula::Forall(v, and_to_or(l))),
        Formula::Exists(v, l) => boxed!(Formula::Exists(v, and_to_or(l))),
    }
}

pub fn is_valid(formula: Formula) -> bool {
    println!("{}", formula);
    // let example_conversion = and_to_or(boxed!(formula));
    // println!("{}", example_conversion);
    let negated = Formula::Neg(boxed!(formula));
    let clausal = clausal::to_clausal(negated);
    resolution::resolve(clausal)
}
