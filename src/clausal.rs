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

fn to_clausal_form(formula: Formula) -> Formula {
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
    #[test]
    fn test_to_clausal() {
        // example test
    }
}
