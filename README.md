# Theorem Prover for First-Order Logic

This project stemmed from an open-ended final project for a graduate course, Formal Verification (CMPT 777). We chose this specific project because it seemed the most interesting. For a description of the code and algorithm, refer to [report.pdf](report.pdf). The contributors are Jimmy Chen Chen (main), Khang Le (major), and Nazanin Yousefian (minor).

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
