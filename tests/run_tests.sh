#!/bin/bash

# Exit on any error
set -e

# Change to the directory of the script
cd "$(dirname "$0")"

echo "Running all tests in tests..."
echo "--------------------------------------"

# Count variables
TOTAL=0
PASSED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Iterate over all test files
for test_file in test-*.scm; do
    echo "Running $test_file..."
    
    # Run the test and capture output
    OUTPUT=$(../build/nanos --boot ../boot/core.ir --script "$test_file" 2>&1)
    RETVAL=$?
    
    echo "$OUTPUT"
    
    if [ $RETVAL -ne 0 ]; then
        echo "--------------------------------------"
        echo -e "${RED}ERROR: $test_file exited with non-zero status $RETVAL${NC}"
        exit 1
    fi
    
    if echo "$OUTPUT" | grep -q "FAIL"; then
        echo "--------------------------------------"
        echo -e "${RED}ERROR: Failures detected in $test_file${NC}"
        exit 1
    fi

    if echo "$OUTPUT" | grep -q "Error:"; then
        echo "--------------------------------------"
        echo -e "${RED}ERROR: Error detected in $test_file${NC}"
        exit 1
    fi

    TOTAL=$((TOTAL + 1))
    echo "Done with $test_file"
    echo "--------------------------------------"
done

echo -e "${GREEN}All $TOTAL test files passed successfully!${NC}"
