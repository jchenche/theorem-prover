use theorem_prover::{lang::{Formula::*, Term::*, Pred, Var}, boxed};

fn main() {
    // TODO: Parse input formula into F of type Formula and feed it into is_valid()

    // TODO: maybe use macros to make it easier to read and write formulas
    // F : ¬(∃y.(∀z.((p(z, y)) ↔ (¬(∃x.((p(z, x)) ∧ (p(x, z))))))))
    let pzy = Pred(Pred::new("p", vec![boxed!(Var(Var::new("z"))), boxed!(Var(Var::new("y")))]));
    let pzx = Pred(Pred::new("p", vec![boxed!(Var(Var::new("z"))), boxed!(Var(Var::new("x")))]));
    let pxz = Pred(Pred::new("p", vec![boxed!(Var(Var::new("x"))), boxed!(Var(Var::new("z")))]));
    let p_conj = And(boxed!(pzx), boxed!(pxz));
    let formula =
        Neg(boxed!(Exists(Var::new("y"), boxed!(Forall(Var::new("z"),
            boxed!(Iff(
                boxed!(pzy),
                boxed!(Neg(boxed!(Exists(Var::new("x"), boxed!(p_conj))))))
        ))))));

    theorem_prover::is_valid(formula);
}
