use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use pest::Parser;
use theorem_prover::lang::{Formula, Fun, Obj, Pred, Term, Var};

#[derive(pest_derive::Parser)]
#[grammar = "first_order.pest"]
pub struct FormulaParser;

pub fn parse(formula: String) -> Option<Formula> {
    println!("=== Formula {formula} ===");
    let pratt = create_pratt_parser();
    match FormulaParser::parse(Rule::program, &formula) {
        Ok(mut pairs) => Some(parse_formula(pairs.next().unwrap().into_inner(), &pratt)),
        Err(e) => {
            eprintln!("Parse failed: {:?} (moving on to the next formula)", e);
            None
        }
    }
}

pub fn parse_formula(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Formula {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::predicate => {
                let mut predicate_id = "";
                let mut predicate_args = vec![];
                for pair in primary.into_inner() {
                    match pair.as_rule() {
                        Rule::predicate_id => predicate_id = pair.as_str(),
                        Rule::args => predicate_args = parse_term(pair.into_inner()),
                        rule => {
                            unreachable!("Parser expected predicate id or terms, found {:?}", rule)
                        }
                    }
                }
                assert!(predicate_id != "", "Predicates must have an id");
                Formula::Pred(Pred::new(predicate_id, Box::new(predicate_args)))
            }
            Rule::top => Formula::True,
            Rule::bottom => Formula::False,
            Rule::formula => parse_formula(primary.into_inner(), pratt),
            rule => unreachable!("Parser expected atom, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::iff => Formula::Iff(Box::new(lhs), Box::new(rhs)),
            Rule::imply => Formula::Imply(Box::new(lhs), Box::new(rhs)),
            Rule::or => Formula::Or(Box::new(lhs), Box::new(rhs)),
            Rule::and => Formula::And(Box::new(lhs), Box::new(rhs)),
            rule => unreachable!("Parser expected infix operation, found {:?}", rule),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => Formula::Neg(Box::new(rhs)),
            Rule::forall => {
                if let Some(v) = op.into_inner().next() {
                    match v.as_rule() {
                        Rule::variable => Formula::Forall(Var::new(v.as_str()), Box::new(rhs)),
                        rule => {
                            unreachable!("Parser expected variable for forall, found {:?}", rule)
                        }
                    }
                } else {
                    unreachable!("Parser expected variable for forall")
                }
            }
            Rule::exists => {
                if let Some(v) = op.into_inner().next() {
                    match v.as_rule() {
                        Rule::variable => Formula::Exists(Var::new(v.as_str()), Box::new(rhs)),
                        rule => {
                            unreachable!("Parser expected variable for exists, found {:?}", rule)
                        }
                    }
                } else {
                    unreachable!("Parser expected variable for exists")
                }
            }
            rule => unreachable!("Parser expected prefix operation, found {:?}", rule),
        })
        .parse(pairs)
}

fn parse_term(pairs: Pairs<Rule>) -> Vec<Term> {
    let mut args = vec![];
    for pair in pairs {
        match pair.as_rule() {
            Rule::variable => args.push(Term::Var(Var::new(pair.as_str()))),
            Rule::constant => args.push(Term::Obj(Obj::new(pair.as_str()))),
            Rule::function => {
                let mut function_id = "";
                let mut function_args = vec![];
                for token in pair.into_inner() {
                    match token.as_rule() {
                        Rule::function_id => function_id = token.as_str(),
                        Rule::args => function_args = parse_term(token.into_inner()),
                        rule => {
                            unreachable!("Parser expected function id or terms, found {:?}", rule)
                        }
                    }
                }
                assert!(function_id != "", "Functions must have an id");
                args.push(Term::Fun(Fun::new(function_id, Box::new(function_args))))
            }
            rule => unreachable!("Parser expected term, found {:?}", rule),
        }
    }
    return args;
}

fn create_pratt_parser() -> PrattParser<Rule> {
    use pest::pratt_parser::{Assoc::*, Op};
    use Rule::*;
    PrattParser::new()
        .op(Op::infix(iff, Left))
        .op(Op::infix(imply, Left))
        .op(Op::infix(or, Left))
        .op(Op::infix(and, Left))
        .op(Op::prefix(neg))
        .op(Op::prefix(forall) | Op::prefix(exists))
}

#[cfg(test)]
mod tests {
    use super::*;
    use theorem_prover::{
        lang::{Formula, Pred, Term, Var},
        And, Exists, Forall, Fun, Iff, Neg, Or, Pred, Var,
    };

    #[test]
    fn test_parse_simple() {
        let raw_formula = String::from(r#"forall z.(~p(x,f(y)) \/ q(z) /\ r())"#);
        let parsed_formula = Forall!(
            "z",
            And!(
                Or!(
                    Neg!(Pred!("p", [Var!("x"), Fun!("f", [Var!("y")])])),
                    Pred!("q", [Var!("z")])
                ),
                Pred!("r", [])
            )
        );
        assert_eq!(parse(raw_formula).unwrap(), parsed_formula);
    }

    #[test]
    fn test_parse_complex() {
        let raw_formula = String::from(
            r#"~(exists y.(forall z.((p(z, y)) <-> (~(exists x.((p(z, x)) /\ (p(x, z))))))))"#,
        );
        let parsed_formula = Neg!(Exists!(
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
        )); // ¬(∃y.(∀z.((p(z, y)) ↔ (¬(∃x.((p(z, x)) ∧ (p(x, z))))))))
        assert_eq!(parse(raw_formula).unwrap(), parsed_formula);
    }
}
