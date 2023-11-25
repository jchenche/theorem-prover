# Theorem Prover for First-Order Logic

Team: Jimmy Chen Chen, Khang Le, Nazanin Yousefian.

Course: Formal Verification (CMPT 777).

We chose this project because it's interesting. For a description of the code and algorithm, refer to report.pdf.

## How to run?

1. Install [Rust](https://www.rust-lang.org/tools/install)
```sh
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

2. Output the help menu
```sh
$ cargo run -- --help
```

3. Run our prover with sample formulas (the result is also published to `output.txt`)
```sh
$ cargo run -- formulas.txt
```

4. Run unit tests (take up to 5 seconds)
```sh
$ cargo test
```

5. Run end-to-end tests (take up to 60 seconds)
```sh
$ ./e2e_test.sh
```
