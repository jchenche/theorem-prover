use clap::Parser;
use std::path::PathBuf;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};
use theorem_prover::lang::Formula;

mod parsing;

/// A theorem prover that checks the validity of first-order formulas
#[derive(Parser)]
#[command(name = "prover", version)]
struct Prover {
    /// File that contains formulas, separated by lines
    #[arg(value_name = "FILE_PATH")]
    formulas: PathBuf,

    /// Set a time limit for the execution
    #[arg(short, long, value_name = "MINUTE", default_value_t = 1)]
    limit: i32,
}

fn main() {
    let prover = Prover::parse();
    let formulas = get_formulas(prover.formulas);
    for formula in formulas {
        print!("{formula}");
        if theorem_prover::is_valid(formula) {
            println!(" is valid!");
        } else {
            println!(" may be valid or invalid. Since first order logic is undecidable, the program may run forever, so it can't tell us.");
        }
    }
}

fn get_formulas(file_path: PathBuf) -> Vec<Formula> {
    let mut formulas = vec![];
    let file = File::open(file_path).expect("Failed to open the file");
    let reader = BufReader::new(file);
    for line in reader.lines() {
        match parsing::parse(line.expect("Failed to read the file")) {
            Some(formula) => formulas.push(formula),
            None => {}
        }
    }
    return formulas;
}
