#!/bin/bash

# Script to add main functions to LX examples that don't have them

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Ensure we're in the project directory
cd "$PROJECT_DIR"

echo -e "${BLUE}Adding main functions to LX examples...${NC}"

# Counter
ADDED_MAIN=0
ALREADY_HAS_MAIN=0

# Function to add main function to a file
add_main_function() {
    local file_path="$1"
    local relative_path="${file_path#$PROJECT_DIR/}"

    # Check if file already has main function
    if grep -q "def main()" "$file_path"; then
        echo -e "${GREEN}✓ $relative_path already has main function${NC}"
        ALREADY_HAS_MAIN=$((ALREADY_HAS_MAIN + 1))
        return 0
    fi

    # Find the first function name to call in main
    local first_function=$(grep "^def " "$file_path" | head -1 | sed 's/^def \([^(]*\).*/\1/')

    if [ -z "$first_function" ]; then
        echo -e "${YELLOW}⚠ $relative_path has no functions to call${NC}"
        return 0
    fi

    # Add main function at the end of the file
    echo "" >> "$file_path"
    echo "def main() do" >> "$file_path"
    echo "    $first_function()" >> "$file_path"
    echo "end" >> "$file_path"

    echo -e "${BLUE}+ $relative_path: added main() -> $first_function()${NC}"
    ADDED_MAIN=$((ADDED_MAIN + 1))
}

# Process all task directories
for task_dir in examples/task_*; do
    if [ -d "$task_dir" ]; then
        echo -e "\n${BLUE}Processing $task_dir...${NC}"
        for lx_file in "$task_dir"/*.lx; do
            if [ -f "$lx_file" ]; then
                add_main_function "$lx_file"
            fi
        done
    fi
done

# Process root examples directory
if [ -d "examples" ]; then
    echo -e "\n${BLUE}Processing root examples...${NC}"
    for lx_file in examples/*.lx; do
        if [ -f "$lx_file" ]; then
            add_main_function "$lx_file"
        fi
    done
fi

# Summary
echo -e "\n${BLUE}=================================================="
echo "SUMMARY"
echo "==================================================${NC}"
echo -e "${GREEN}✓ Already had main: $ALREADY_HAS_MAIN${NC}"
echo -e "${BLUE}+ Added main: $ADDED_MAIN${NC}"
echo -e "${BLUE}📊 Total processed: $((ALREADY_HAS_MAIN + ADDED_MAIN))${NC}"

if [ $ADDED_MAIN -gt 0 ]; then
    echo -e "\n${GREEN}Main functions added successfully!${NC}"
else
    echo -e "\n${GREEN}All examples already have main functions!${NC}"
fi