#!/bin/bash

# LX Examples Validation Script
# This script compiles and runs all LX examples to validate the compiler

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
WHITE='\033[1;37m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Ensure we're in the project directory
cd "$PROJECT_DIR"

# Check if lx1 compiler exists
if [ ! -f "./lx1" ]; then
    echo -e "${RED}Error: lx1 compiler not found. Run 'make compile' first.${NC}"
    exit 1
fi

# Check if erlang is available
if ! command -v erlc &> /dev/null; then
    echo -e "${RED}Error: erlc (Erlang compiler) not found. Please install Erlang.${NC}"
    exit 1
fi

echo -e "${BLUE}Starting LX examples validation...${NC}"
echo "=================================================="

# Arrays to store results
SUCCESSFUL_EXAMPLES=()
FAILED_EXAMPLES=()
EXPECTED_FAILURES=()

# Function to test a single example
test_example() {
    local file_path="$1"
    local relative_path="${file_path#$PROJECT_DIR/}"
    local example_name=$(basename "$file_path" .lx)

    # Check if this is an expected failure test
    local is_expected_failure=false
    if [[ "$example_name" == fail_* ]]; then
        is_expected_failure=true
    fi

    echo -en "Testing ${WHITE}$relative_path${NC}... "

    # Check if file has main function
    if ! grep -q "def main()" "$file_path"; then
        echo -e "${RED}✗ No main function found${NC}"
        if [ "$is_expected_failure" = true ]; then
            EXPECTED_FAILURES+=("$relative_path (no main function)")
        else
            FAILED_EXAMPLES+=("$relative_path (no main function)")
        fi
        return
    fi

    # Try to compile and run the example
    if timeout 10s ./lx1 run "$file_path" >/dev/null 2>&1; then
        if [ "$is_expected_failure" = true ]; then
            echo -e "${YELLOW}⚠ Expected to fail but succeeded${NC}"
            FAILED_EXAMPLES+=("$relative_path (expected failure but succeeded)")
        else
            echo -e "${GREEN}✓ Success${NC}"
            SUCCESSFUL_EXAMPLES+=("$relative_path")
        fi
    else
        if [ "$is_expected_failure" = true ]; then
            echo -e "${GREEN}✓ Expected failure (test passed)${NC}"
            EXPECTED_FAILURES+=("$relative_path")
        else
            echo -e "${RED}✗ Failed${NC}"
            FAILED_EXAMPLES+=("$relative_path")
        fi
    fi
}

# Find all .lx files in examples directory
echo "Scanning for LX examples..."
LX_FILES=($(find examples -name "*.lx" -type f | sort))

if [ ${#LX_FILES[@]} -eq 0 ]; then
    echo -e "${RED}No .lx files found in examples directory${NC}"
    exit 1
fi

echo "Found ${#LX_FILES[@]} LX examples"
echo "=================================================="

# Test each example
for file_path in "${LX_FILES[@]}"; do
    test_example "$file_path"
done

echo "=================================================="
echo -e "${BLUE}Validation Complete!${NC}"
echo ""

# Generate summary report
echo -e "${BLUE}📊 VALIDATION REPORT${NC}"
echo "=================================================="

# Count results
TOTAL_EXAMPLES=${#LX_FILES[@]}
SUCCESS_COUNT=${#SUCCESSFUL_EXAMPLES[@]}
FAILED_COUNT=${#FAILED_EXAMPLES[@]}
EXPECTED_FAIL_COUNT=${#EXPECTED_FAILURES[@]}
ACTUAL_SUCCESS_COUNT=$((SUCCESS_COUNT + EXPECTED_FAIL_COUNT))

echo -e "Total examples processed: ${WHITE}$TOTAL_EXAMPLES${NC}"
echo -e "✅ Successful: ${GREEN}$SUCCESS_COUNT${NC}"
echo -e "⚠️  Expected failures (test passed): ${GREEN}$EXPECTED_FAIL_COUNT${NC}"
echo -e "❌ Failed: ${RED}$FAILED_COUNT${NC}"
echo -e "📈 Overall success rate: ${WHITE}$((ACTUAL_SUCCESS_COUNT * 100 / TOTAL_EXAMPLES))%${NC}"
echo ""

# Show successful examples
if [ ${#SUCCESSFUL_EXAMPLES[@]} -gt 0 ]; then
    echo -e "${GREEN}✅ SUCCESSFUL EXAMPLES:${NC}"
    for example in "${SUCCESSFUL_EXAMPLES[@]}"; do
        echo -e "  ${GREEN}✓${NC} $example"
    done
    echo ""
fi

# Show expected failures (these are actually test successes)
if [ ${#EXPECTED_FAILURES[@]} -gt 0 ]; then
    echo -e "${GREEN}⚠️  EXPECTED FAILURES (TESTS PASSED):${NC}"
    for example in "${EXPECTED_FAILURES[@]}"; do
        echo -e "  ${GREEN}✓${NC} $example"
    done
    echo ""
fi

# Show failed examples
if [ ${#FAILED_EXAMPLES[@]} -gt 0 ]; then
    echo -e "${RED}❌ FAILED EXAMPLES:${NC}"
    for example in "${FAILED_EXAMPLES[@]}"; do
        echo -e "  ${RED}✗${NC} $example"
    done
    echo ""
fi

# Exit with error if there are unexpected failures
if [ $FAILED_COUNT -gt 0 ]; then
    echo -e "${RED}Validation completed with $FAILED_COUNT unexpected failures.${NC}"
    exit 1
else
    echo -e "${GREEN}All examples validated successfully!${NC}"
    exit 0
fi