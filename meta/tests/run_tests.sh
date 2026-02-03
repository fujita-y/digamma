#!/bin/bash

# Exit on any error
set -e

# Change to the directory of the script
cd "$(dirname "$0")"

echo "Running all tests in meta/tests..."
echo "--------------------------------------"

# Count variables
TOTAL=0
PASSED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Iterate over all test files
for test_file in test_*.scm; do
    echo "Running $test_file..."
    
    # Run the test and capture output
    # -I.. allows loading files from meta/
    # -I. allows loading files from meta/tests/
    # Also include ../../core for cross dependencies if any (though user said use files under core/ or meta/ only)
    # We'll just stick to -I.. -I. for now and -I../../core if needed by specific tests, 
    # but based on the request "files under core/ and meta/ do not reference any file user research/ but use files under core/ or meta/ only"
    # existing code might need updates.
    OUTPUT=$(gosh -I.. -I. -I../../core "$test_file" 2>&1)
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
    
    TOTAL=$((TOTAL + 1))
    echo "Done with $test_file"
    echo "--------------------------------------"
done

echo -e "${GREEN}All $TOTAL test files passed successfully!${NC}"
