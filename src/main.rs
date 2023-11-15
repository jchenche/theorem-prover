use theorem_prover::{lang::{Formula, Pred, Term, Var}, Neg, Exists, Forall, Iff, And, Pred, Var};

mod parsing;

fn main() {
    // TODO: Parse input formula into F of type Formula and feed it into is_valid(). We may assume input follows a form:
    // Symbols are forall, exists, ~, /\, \/, ->, <->, true, false, and arbitrary letters p, f, c, x, Jane, etc...
    // Every connectives always wrap their subformulas with parenthesis so we don't have to deal with operator precedence
    // F : ¬(∃y.(∀z.((p(z, y)) ↔ (¬(∃x.((p(z, x)) ∧ (p(x, z))))))))
    let formula =
        Neg!(Exists!("y", Forall!("z",
            Iff!(
                Pred!("p", ["z", "y"]),
                Neg!(Exists!("x", And!(Pred!("p", ["z", "x"]), Pred!("p", ["x", "z"]))))
            )
        )));

    println!("{formula}");
    if theorem_prover::is_valid(formula) {
        println!(" is valid!");
    } else {
        println!(" may be valid or invalid. Since first order logic is undecidable, we sometimes don't know.");
    }
}
