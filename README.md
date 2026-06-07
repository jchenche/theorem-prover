# Theorem Prover for First-Order Logic

A theorem prover for the full version of first-order logic, available both as a binary and library. For a description of the code and algorithm, refer to [report.pdf](report.pdf). The repository is at https://github.com/jchenche/theorem-prover.

## Usage

- Output the help menu
```sh
$ cargo run -- --help
```

- Run our prover with sample formulas (refer to [first_order.pest](first_order.pest) or [report.pdf](report.pdf) for the syntax to write your own)
```sh
$ cargo run -- formulas.txt
```

- Run unit tests (5 seconds)
```sh
$ cargo test
```

- Run end-to-end tests (60 seconds)
```sh
$ ./e2e_test.sh
```
