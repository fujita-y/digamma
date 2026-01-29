#!/bin/bash

# Exit on any error
set -e

# Change to the project root relative to the script
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

echo "Running all tests in research/repl..."
echo "--------------------------------------"

# Count variables
TOTAL=0
PASSED=0

# Iterate over all test files in research/repl
for test_file in research/repl/test_*.scm; do
    echo "Running $test_file..."
    
    # Run the test and capture output
    # We use a temporary file to capture output so we can check for "FAIL"
    # because Gauche might not return non-zero exit code on logical failures
    # Added -I. to allow loading files from the current directory
    OUTPUT=$(gosh -I. "$test_file" 2>&1)
    RETVAL=$?
    
    echo "$OUTPUT"
    
    if [ $RETVAL -ne 0 ]; then
        echo "--------------------------------------"
        echo "ERROR: $test_file exited with non-zero status $RETVAL"
        exit 1
    fi
    
    if echo "$OUTPUT" | grep -q "FAIL"; then
        echo "--------------------------------------"
        echo "ERROR: Failures detected in $test_file"
        exit 1
    fi
    
    TOTAL=$((TOTAL + 1))
    echo "Done with $test_file"
    echo "--------------------------------------"
done

echo "All $TOTAL test files passed successfully!"
