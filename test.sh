#!/bin/bash
# test.sh - Test suite for ascii-tree

set -e # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Track test results
TESTS_PASSED=0
TESTS_FAILED=0

print_header() {
    echo -e "\n${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}\n"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

print_failure() {
    echo -e "${RED}✗${NC} $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

print_info() {
    echo -e "${YELLOW}ℹ${NC} $1"
}

run_test() {
    print_header "$1"
    if emacs --batch -L . -l ert -l ascii-tree.el -l "$2" \
        -f ert-run-tests-batch-and-exit 2>&1 | tee /tmp/test-ert.log | grep -q "0 unexpected"; then
        PASSED=$(grep "Ran.*tests" /tmp/test-ert.log | grep -o '[0-9]\+' | head -1)
        print_success "All $PASSED ERT unit tests passed"
    else
        print_failure "ERT unit tests failed"
        cat /tmp/test-ert.log
    fi
}

run_test "Running Basic ERT Unit Tests" tests/test-basic.el
run_test "Running idempotency ERT Unit Tests" tests/test-idempotency.el
run_test "Running Whole Buffer ERT Unit Tests" tests/test-whole-buffer.el

# Test Summary
print_header "Test Summary"
TOTAL_TESTS=$((TESTS_PASSED + TESTS_FAILED))
echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✓ All tests passed!${NC}\n"
    exit 0
else
    echo -e "\n${RED}✗ Some tests failed${NC}\n"
    exit 1
fi
