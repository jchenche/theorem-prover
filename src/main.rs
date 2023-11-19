use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
};
use theorem_prover::lang::Formula;

mod parsing;

fn main() {
    let formulas = get_formulas();
    for formula in formulas {
        print!("{formula}");
        if theorem_prover::is_valid(formula) {
            println!(" is valid!");
        } else {
            println!(" may be valid or invalid. Since first order logic is undecidable, the program may run forever, so it can't tell us.");
        }
    }
}

fn get_formulas() -> Vec<Formula> {
    let mut formulas = vec![];
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Please provide a file path containing formulas");
        std::process::exit(1);
    }
    let file_path = &args[1];
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
