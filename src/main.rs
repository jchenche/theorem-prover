use clap::Parser;
use log::trace;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};
use theorem_prover::{lang::Formula, ProverError};

mod parsing;

/// A theorem prover that checks the validity of first-order formulas
#[derive(Parser)]
#[command(name = "prover", version)]
struct Prover {
    /// File that contains formulas, separated by lines
    #[arg(value_name = "FILE_PATH")]
    formulas: PathBuf,

    /// Set a time limit for the execution
    #[arg(short, long, value_name = "SECOND", default_value_t = 60)]
    limit: u64,
}

fn main() {
    log4rs::init_file("log4rs.yml", Default::default()).unwrap();
    trace!("============================== BEGIN prover ==============================");

    let prover = Prover::parse();
    let formulas = get_formulas(prover.formulas);

    let mut output = String::new();
    for (index, formula) in formulas.into_iter().enumerate() {
        trace!("---------- Proving {formula} ----------");
        if index != 0 {
            output += "\n";
        }
        output += formula.to_string().as_str();
        match theorem_prover::is_valid(formula, prover.limit) {
            Ok(true) => output += " is valid.",
            Ok(false) => output += " is invalid.",
            Err(ProverError::TimeoutError) => output += " may be valid or invalid. Since first order logic is undecidable, the program may run forever, so it can't tell us.",
            Err(ProverError::ArityError) => output += ". Same predicates or functions must have the same signature/arity. Skipping this formula...",
        }
    }
    println!("{output}");

    // write the output string to file for comparison
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("output.txt")
        .expect("Unable to open file for output");
    file.write_all(output.as_bytes()).unwrap();

    trace!("============================== END prover ==============================");
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
