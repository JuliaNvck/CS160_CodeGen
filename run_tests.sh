#!/bin/bash

# Test script for CS160 codegen project
# Runs all tests in student-tests/ts1 and compares output with solutions

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if codegen executable exists
if [ ! -f "./codegen" ]; then
    echo -e "${RED}Error: './codegen' executable not found. Run 'make' first.${NC}"
    exit 1
fi

# Test directory
TEST_DIR="student-tests/ts1"

if [ ! -d "$TEST_DIR" ]; then
    echo -e "${RED}Error: Test directory '$TEST_DIR' not found.${NC}"
    exit 1
fi

# Counters
passed=0
failed=0
errors=0

# Array to store failed test names
failed_tests=()
error_tests=()

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Running tests from $TEST_DIR${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Find all .lirj files and sort them numerically
for test_file in $(ls "$TEST_DIR"/*.lirj | sort -V); do
    # Extract test name (e.g., valid.0 from valid.0.lirj)
    base_name=$(basename "$test_file" .lirj)
    solution_file="$TEST_DIR/${base_name}.soln"
    
    # Check if solution file exists
    if [ ! -f "$solution_file" ]; then
        echo -e "${YELLOW}SKIP${NC} $base_name (no solution file)"
        continue
    fi
    
    # Run the codegen and capture output
    output=$(./codegen "$test_file" 2>&1)
    exit_code=$?
    
    # Check if the program crashed or had an error
    if [ $exit_code -ne 0 ]; then
        echo -e "${RED}ERROR${NC} $base_name (exit code: $exit_code)"
        error_tests+=("$base_name")
        ((errors++))
        continue
    fi
    
    # Read the expected solution
    expected=$(cat "$solution_file")
    
    # Compare output with solution
    if [ "$output" == "$expected" ]; then
        echo -e "${GREEN}PASS${NC} $base_name"
        ((passed++))
    else
        echo -e "${RED}FAIL${NC} $base_name"
        failed_tests+=("$base_name")
        ((failed++))
    fi
done

# Print summary
echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}Passed: $passed${NC}"
echo -e "${RED}Failed: $failed${NC}"
echo -e "${YELLOW}Errors: $errors${NC}"
total=$((passed + failed + errors))
echo -e "Total:  $total"
echo ""

# Show details for failed tests
if [ $failed -gt 0 ]; then
    echo -e "${RED}========================================${NC}"
    echo -e "${RED}Failed Tests (Output Mismatch)${NC}"
    echo -e "${RED}========================================${NC}"
    echo ""
    
    for test_name in "${failed_tests[@]}"; do
        test_file="$TEST_DIR/${test_name}.lirj"
        solution_file="$TEST_DIR/${test_name}.soln"
        
        echo -e "${YELLOW}--- Test: $test_name ---${NC}"
        echo ""
        
        # Run again to get output
        actual_output=$(./codegen "$test_file" 2>&1)
        expected_output=$(cat "$solution_file")
        
        echo -e "${BLUE}Expected:${NC}"
        echo "$expected_output"
        echo ""
        echo -e "${BLUE}Actual:${NC}"
        echo "$actual_output"
        echo ""
        echo -e "${BLUE}Diff:${NC}"
        diff -u <(echo "$expected_output") <(echo "$actual_output") || true
        echo ""
        echo "----------------------------------------"
        echo ""
    done
fi

# Show details for error tests
if [ $errors -gt 0 ]; then
    echo -e "${YELLOW}========================================${NC}"
    echo -e "${YELLOW}Error Tests (Crashed/Exception)${NC}"
    echo -e "${YELLOW}========================================${NC}"
    echo ""
    
    for test_name in "${error_tests[@]}"; do
        test_file="$TEST_DIR/${test_name}.lirj"
        
        echo -e "${YELLOW}--- Test: $test_name ---${NC}"
        echo ""
        
        # Run again to show error
        echo -e "${BLUE}Error output:${NC}"
        ./codegen "$test_file" 2>&1 || true
        echo ""
        echo "----------------------------------------"
        echo ""
    done
fi

# Exit with failure if any tests failed or errored
if [ $failed -gt 0 ] || [ $errors -gt 0 ]; then
    exit 1
else
    exit 0
fi
