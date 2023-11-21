#!/bin/bash

# Build and run the Rust program using cargo
OUTPUT=$(cargo run -- ./test_formulas.txt)

# Paths to actual and expected outputs
actual_output="./output.txt"
expected_output="./expected_output.txt"

# Check if the actual output matches the expected output
if cmp -s "$actual_output" "$expected_output"; then
    echo "Test passed: Output matches the expected result"
else
    echo "Test failed: Output does not match the expected result"
fi