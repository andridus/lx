# LX Examples Validation System

This document explains how to use the validation system for LX examples, which compiles and runs all examples to ensure the compiler works correctly.

## Overview

The validation system consists of:
- **Makefile** with targets for building, testing, and validation
- **Scripts** for automatic main function addition and example validation
- **End-to-end testing** that compiles and runs all examples

## Prerequisites

1. **V compiler** installed and available in PATH
2. **Erlang** installed with `erlc` and `erl` commands available
3. **Bash** shell (for running validation scripts)

## Quick Start

### 1. Build the LX compiler
```bash
cd lx
make compile
```

### 2. Run end-to-end validation
```bash
make e2e-test
```

This will:
- Automatically add `main()` functions to examples that don't have them
- Compile and run all examples using `./lx1 run <file.lx>`
- Generate a comprehensive report of successes and failures

## Available Makefile Targets

### `make compile`
Builds the LX compiler from source.

### `make test`
Runs the LX compiler's internal test suite.

### `make add-main`
Automatically adds `main()` functions to all examples that don't have them.
- Scans all `examples/task_*/` directories
- Finds the first function in each `.lx` file
- Adds `def main() do first_function() end`

### `make e2e-test`
Runs the complete validation suite:
1. Ensures all examples have `main()` functions
2. Compiles and runs each example
3. Generates a detailed report

### `make clean`
Removes generated files (`.erl`, `.beam`, crash dumps).

### `make help`
Shows available targets and usage examples.

## Manual Script Usage

### Add main functions manually
```bash
./scripts/add_main_functions.sh
```

### Run validation manually
```bash
./scripts/validate_examples.sh
```

## How It Works

### 1. Main Function Detection
The system checks each `.lx` file for a `def main()` function. Examples without `main()` are either:
- **Skipped** during validation, or
- **Automatically fixed** by the `add-main` target

### 2. Compilation and Execution
For each valid example:
1. **Compile**: `./lx1 <file.lx>` generates `.erl` file
2. **Erlang compile**: `erlc <file>.erl` generates `.beam`
3. **Execute**: `./lx1 run <file.lx>` runs the example
4. **Cleanup**: Removes generated `.erl` and `.beam` files

### 3. Reporting
The validation generates a comprehensive report:
- ✅ **SUCCESSFUL**: Examples that compiled and ran successfully
- ❌ **FAILED**: Examples that had compilation or runtime errors
- ⚠️ **SKIPPED**: Examples without `main()` functions
- 📊 **Statistics**: Total count, success rate, and detailed lists

## Example Output

```
Starting LX examples validation...
==================================================

Validating Task: task_01
--------------------------------------------------
Testing examples/task_01/simple.lx... SUCCESS

Validating Task: task_02
--------------------------------------------------
Testing examples/task_02/simple_binding.lx... SUCCESS
Testing examples/task_02/multiple_bindings.lx... SUCCESS

==================================================
VALIDATION SUMMARY REPORT
==================================================

✅ SUCCESSFUL: 15
  • examples/task_01/simple.lx
  • examples/task_02/simple_binding.lx
  • examples/task_03/simple_arithmetic.lx
  ...

📊 TOTAL EXAMPLES PROCESSED: 15
📈 SUCCESS RATE: 100%

All examples validated successfully! 🎉
```

## Troubleshooting

### Common Issues

1. **"lx1 compiler not found"**
   - Run `make compile` first

2. **"erlc not found"**
   - Install Erlang/OTP

3. **Examples failing**
   - Check that examples have valid LX syntax
   - Ensure `main()` functions call existing functions
   - Verify Erlang dependencies are available

### Debug Mode

To see detailed output during validation, modify the script:
```bash
# In validate_examples.sh, change this line:
if ./lx1 run "$file_path" > /dev/null 2>&1; then
# To:
if ./lx1 run "$file_path"; then
```

## Adding New Examples

1. **Create** your `.lx` file in the appropriate `examples/task_XX/` directory
2. **Include** a `def main()` function that calls your example function
3. **Test** with `make e2e-test`

Example:
```lx
def my_example_function() do
    # Your example code here
    42
end

def main() do
    my_example_function()
end
```

## Continuous Integration

The validation system is designed for CI/CD:
- **Exit codes**: 0 for success, 1 for failures
- **Comprehensive reporting**: Easy to parse for CI tools
- **Automatic cleanup**: No leftover files
- **Fast execution**: Parallel processing where possible

Use in CI:
```bash
make e2e-test
# Exit code will indicate success/failure
```