# LX Compiler - Architecture Documentation

The LX compiler is a complete compiler implementation written in V that translates LX language source code to Erlang. This document provides an overview of the compiler's architecture and the responsibility of each component.

## Features

### Directives System

LX supports compile-time directives that provide metadata and control compilation behavior. Directives are prefixed with `@` and must be placed immediately before function definitions.

#### Available Directives

- **`@reflection`**: Prints detailed type information for function parameters, guards, and body expressions during compilation
- **`@inline`**: Marks a function for inlining optimization (planned)
- **`@deprecated`**: Marks a function as deprecated (planned)

#### Using the Reflection Directive

The `@reflection` directive is particularly useful for debugging type inference and understanding how the compiler interprets your code:

```lx
@reflection
def add(a, b) do
  a + b
end
```

When compiled, this will output detailed type information including:
- Parameter types inferred by the compiler
- Guard expression types
- Body expression types with line and column information
- Variable bindings and their inferred types

This is especially helpful when:
- Debugging type inference issues
- Understanding how the compiler interprets complex expressions
- Learning about the type system behavior
- Verifying that your code is being analyzed correctly

## Project Structure

```
lxc/
├── main.v                 # Main entry point and CLI interface
├── compiler.v             # Core compiler orchestration
├── v.mod                  # V module dependencies
├── lxc                    # Compiled binary executable
│
├── ast/                   # Abstract Syntax Tree definitions
│   ├── ast.v             # Core AST structures and expressions
│   ├── position.v        # Source position tracking
│   └── types.v           # Type system definitions
│
├── frontend/              # Frontend compilation phases
│   ├── lexer/            # Lexical analysis
│   │   ├── lexer.v       # Main lexer implementation
│   │   ├── tokens.v      # Token definitions and types
│   │   ├── keywords.v    # Keyword recognition
│   │   ├── operators.v   # Operator definitions
│   │   ├── states.v      # Lexer state machine
│   │   └── transitions.v # State transition rules
│   │
│   └── parser/           # Syntax analysis
│       ├── main_parser.v # Public parser interface
│       └── internal/     # Internal parser implementation
│           ├── core_parser.v              # Base parser infrastructure
│           ├── error_recovery.v           # Error recovery mechanisms
│           ├── operator_precedence.v      # Operator precedence rules
│           ├── expr_*.v                   # Expression parsing modules
│           ├── stmt_*.v                   # Statement parsing modules
│           └── main_parser_internal.v     # Main parser implementation
│
├── analysis/              # Semantic analysis phases
│   ├── linter/           # Code linting and style checking
│   │   └── linter.v      # Linter implementation
│   │
│   └── typechecker/      # Type checking and inference
│       ├── context.v     # Type context management
│       ├── types.v       # Type system implementation (Hindley-Milner)
│       ├── substitution.v # Type substitution operations
│       └── unification.v # Type unification algorithm
│
├── backend/               # Code generation and optimization
│   ├── codegen/          # Code generation infrastructure
│   │   ├── base.v        # Base code generator interface
│   │   └── mod.v         # Code generation module
│   │
│   ├── erlang/           # Erlang-specific code generation
│   │   ├── generator.v   # Main Erlang code generator
│   │   ├── expressions.v # Expression code generation
│   │   ├── statements.v  # Statement code generation
│   │   ├── patterns.v    # Pattern matching code generation
│   │   ├── formatting.v  # Code formatting utilities
│   │   └── mod.v         # Erlang module
│   │
│   └── optimization/     # Code optimization passes
│       └── mod.v         # Optimization module
│
├── errors/                # Error handling and reporting
│   ├── errors.v          # Error type definitions with categories
│   ├── formatter.v       # Error formatting with context and suggestions
│   └── suggestions.v     # Contextual error correction suggestions
│
├── utils/                 # Utility functions and helpers
│   ├── debug.v           # Debugging utilities
│   ├── file.v            # File handling utilities
│   └── string.v          # String manipulation utilities
│
├── examples/              # Example LX programs
│
└── tests/                 # Test suite
    ├── generate/          # Code generation tests
    │   ├── codegen_test.v # Code generation test suite
    │   ├── fixtures/      # Test input files
    │   └── assets/        # Expected output files
    ├── typecheck/         # Type checking tests
    └── *.v                # Various test modules
```

## Component Responsibilities

### Entry Points
- **`main.v`**: CLI interface, argument parsing, and main execution flow
- **`compiler.v`**: Orchestrates the entire compilation process from source to target

### Abstract Syntax Tree (AST)
- **`ast/ast.v`**: Defines all AST node types for expressions, statements, and patterns
- **`ast/position.v`**: Source position tracking for error reporting
- **`ast/types.v`**: Type system definitions and literal value representations

### Frontend - Lexical Analysis
- **`lexer/lexer.v`**: Main lexer implementation with state machine
- **`lexer/tokens.v`**: Token type definitions, structs with enum values, and helper functions (`keyword()`, `operator()`, `punctuation()`)
- **`lexer/keywords.v`**: Keyword recognition and validation using `KeywordValue` enum
- **`lexer/operators.v`**: Operator definitions and precedence rules using `OperatorValue` enum
- **`lexer/states.v`**: Lexer state definitions and transitions
- **`lexer/transitions.v`**: State transition rules and conditions

**Token Structure**: The lexer uses a consistent token structure where `KeywordToken`, `OperatorToken`, and `PunctuationToken` are structs containing a `value` field with the corresponding enum (`KeywordValue`, `OperatorValue`, `PunctuationValue`). Helper functions provide simplified token creation:
- `keyword(.end_)` creates `KeywordToken{ value: KeywordValue.end_ }`
- `operator(.plus)` creates `OperatorToken{ value: OperatorValue.plus }`
- `punctuation(.lparen)` creates `PunctuationToken{ value: PunctuationValue.lparen }`

### Frontend - Syntax Analysis
- **`parser/main_parser.v`**: Public interface for parsing operations
- **`parser/internal/core_parser.v`**: Base parser infrastructure and utilities
- **`parser/internal/error_recovery.v`**: Error recovery and synchronization
- **`parser/internal/operator_precedence.v`**: Operator precedence and associativity
- **`parser/internal/expr_*.v`**: Expression parsing modules (base, comparison, control flow, etc.)
- **`parser/internal/stmt_*.v`**: Statement parsing modules (functions, definitions, etc.)

### Semantic Analysis
- **`analysis/linter/linter.v`**: Code style checking and best practices enforcement
- **`analysis/typechecker/context.v`**: Type context management and scoping
- **`analysis/typechecker/types.v`**: Type system implementation with Hindley-Milner type inference
- **`analysis/typechecker/substitution.v`**: Type substitution operations
- **`analysis/typechecker/unification.v`**: Type unification algorithm

### Backend - Code Generation
- **`backend/codegen/base.v`**: Code generator interface and base functionality
- **`backend/erlang/generator.v`**: Main Erlang code generator
- **`backend/erlang/expressions.v`**: Expression code generation for Erlang
- **`backend/erlang/statements.v`**: Statement code generation for Erlang
- **`backend/erlang/patterns.v`**: Pattern matching code generation
- **`backend/erlang/formatting.v`**: Erlang code formatting utilities

### Error Handling
- **`errors/errors.v`**: Error type definitions with comprehensive categories (Syntax, Type, Pattern, Record, Binary, Guard, Dependency)
- **`errors/formatter.v`**: Error formatting with source context, visual markers, and contextual suggestions
- **`errors/suggestions.v`**: Contextual error correction suggestions with intelligent hints

#### Error Categories
The compiler provides detailed error categorization for better developer experience:

- **Syntax Errors**: Invalid syntax, missing tokens, unexpected characters
- **Type Errors**: Type mismatches, unification failures, inference issues
- **Pattern Errors**: Pattern matching failures, missing fields, invalid patterns
- **Record Errors**: Missing required fields, unknown fields, type mismatches in records
- **Binary Errors**: Binary pattern size mismatches, invalid formats
- **Guard Errors**: Invalid guard expressions, non-boolean conditions
- **Dependency Errors**: Missing modules, version conflicts, import issues

#### Error Formatting Features
- **Contextual Messages**: Clear, descriptive error messages with source location
- **Visual Markers**: Source code highlighting with `^~` indicators
- **Smart Suggestions**: Context-aware suggestions for fixing errors
- **Error Categories**: Clear categorization for different types of errors
- **Color Support**: Optional colored output for better readability

### Utilities
- **`utils/debug.v`**: Debugging utilities and logging
- **`utils/file.v`**: File I/O operations and path handling
- **`utils/string.v`**: String manipulation and formatting utilities

### Testing
- **`tests/generate/`**: Code generation tests with fixtures and expected outputs
- **`tests/typecheck/`**: Type checking and inference tests
- **`tests/*.v`**: Various test modules for different compiler components

#### Writing Tests

The test suite includes utility functions to simplify testing LX to Erlang compilation:

```v
// Test utility function for comparing LX code with generated Erlang
fn assert_lx_generates_erlang(lx_code string, expected_erlang string) {
    // Automatically handles lexing, parsing, and code generation
    // Compares the generated Erlang code with expected output
}

// Example usage in tests
fn test_simple_function() {
    assert_lx_generates_erlang(
        'def f() do\n1\nend',
        '-module(main).\n-export([f/0]).\n\nf() ->\n1.\n'
    )
}
```

This utility function eliminates the need for manual token creation and provides a clean, readable way to test the complete compilation pipeline from LX source to Erlang output.

## Compilation Pipeline

1. **Lexical Analysis**: Source code → Tokens (`frontend/lexer/`)
2. **Syntax Analysis**: Tokens → AST (`frontend/parser/`)
3. **Semantic Analysis**: AST → Type-checked AST (`analysis/`)
4. **Code Generation**: Type-checked AST → Erlang code (`backend/erlang/`)

## Key Design Principles

- **Modularity**: Each component has a single, well-defined responsibility
- **Error Recovery**: Robust error handling with meaningful error messages and suggestions
- **Type Safety**: Strong type system with Hindley-Milner type inference
- **Extensibility**: Clean interfaces allow for easy addition of new features
- **Testability**: Comprehensive test suite covering all major components
- **Developer Experience**: User-friendly error messages with contextual suggestions
- **Separation of Concerns**: Lexer focuses purely on tokenization, parser handles semantic interpretation (e.g., negative numbers are parsed as unary operators, not lexed as negative literals)
- **Modern Compiler Design**: Follows established compiler construction principles with clear phase separation

## Building and Running

```bash
# Build the compiler
v build lxc

# Compile an LX file to Erlang
./lxc compile input.lx

# Run tests
v test lxc
```

## Recent Improvements

### Enhanced Error Handling System
- **Comprehensive Error Categories**: Added support for Pattern, Record, Binary, Guard, and Dependency errors
- **Contextual Suggestions**: Intelligent suggestions for fixing common errors
- **Visual Error Markers**: Source code highlighting with precise error indicators
- **Improved Formatting**: Better error message formatting with categories and context

### Type System Enhancements
- **Hindley-Milner Type Inference**: Complete implementation with type variables and unification
- **Type Substitution**: Robust type substitution operations
- **Type Unification**: Advanced unification algorithm supporting all type constructs
- **Built-in Types**: Complete set of built-in types (integer, float, string, boolean, atom, nil, any, unknown)

### Testing Infrastructure
- **Comprehensive Test Suite**: 20 test modules covering all major components
- **Error Formatting Tests**: Dedicated tests for the enhanced error handling system
- **Type System Tests**: Extensive tests for type inference, unification, and substitution