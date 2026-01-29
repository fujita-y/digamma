#!/bin/bash

# Exit on any error
set -e

# Get the directory where the script is located
DIR="$(cd "$(dirname "$0")" && pwd)"

# List of directories with run_tests.sh
# We exclude "closure" if it doesn't have a run_tests.sh, but we just created it.
DIRS=("syntax" "opt" "vm" "repl" "closure")

FAILED_DIRS=()

echo "======================================================================"
echo "Starting all research tests..."
echo "======================================================================"

for d in "${DIRS[@]}"; do
    if [ -f "$DIR/$d/run_tests.sh" ]; then
        echo ">>> Running tests in research/$d..."
        if ! "$DIR/$d/run_tests.sh"; then
            FAILED_DIRS+=("research/$d")
        fi
        echo ""
    else
        echo ">>> WARNING: No run_tests.sh found in research/$d"
        echo ""
    fi
done

if [ ${#FAILED_DIRS[@]} -ne 0 ]; then
    echo "======================================================================"
    echo "ERROR: Some tests failed in the following directories:"
    for d in "${FAILED_DIRS[@]}"; do
        echo "  - $d"
    done
    exit 1
fi

echo "======================================================================"
echo "All research tests passed successfully!"
echo "======================================================================"
