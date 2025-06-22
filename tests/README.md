# LX-Lang Compiler Test Suite

This directory contains a comprehensive test suite for the LX-Lang compiler, built using the Alcotest testing framework.

## Test Structure

The test suite is organized into several modules:

### 1. Lexer Tests (`test_lexer.ml`)
- Tests tokenization of integers, identifiers, keywords, and operators
- Verifies that the lexer correctly recognizes `let`, `in`, `=`, identifiers, and numeric literals
- Includes tests for string literal tokenization

### 2. Parser Tests (`test_parser.ml`)
- Tests AST generation from token streams
- Verifies parsing of:
  - Integer literals
  - Variables
  - Simple let expressions
  - Nested let expressions
  - Complex expressions

### 3. Compiler Tests (`test_compiler.ml`)
- Tests code generation from AST to Erlang
- Verifies:
  - Integer literal compilation
  - Variable compilation (with capitalization)
  - Let expression compilation
  - String literal compilation
  - Variable name capitalization function
  - End-to-end compilation pipeline

### 4. Example-based Tests (`test_examples.ml`)
- Tests based on the example files in the repository
- Verifies compilation of realistic code examples
- Tests string examples and various expressions
- Includes error handling tests for invalid syntax

## Running Tests

To run the complete test suite:

```bash
make test
```

Or directly with dune:

```bash
dune runtest
```

## Test Coverage

The test suite covers:

- **Lexical Analysis**: All tokens defined in the language
- **Syntax Analysis**: All grammar rules and AST node types
- **Code Generation**: All expression types and their Erlang equivalents
- **End-to-End**: Complete compilation pipeline from source to target
- **Error Handling**: Invalid syntax and edge cases

## Example Files

The test suite includes several example files:

- `example.lx`: Basic let expression (`let x = 42 in x`)
- `example_string.lx`: String literal example (`let greeting = "hello" in greeting`)
- `example_var.lx`: Simple variable (`answer`)

## Test Results

All tests should pass, providing confidence in the compiler's correctness:

```
Test Successful in 0.002s. 22 tests run.
```

## Adding New Tests

To add new tests:

1. Add test functions to the appropriate test module
2. Add the new tests to the `tests` list in that module
3. If creating a new test module, add it to `test_main.ml` and `dune`

## Dependencies

- `alcotest`: Testing framework
- `lx_lib`: The compiler library being tested