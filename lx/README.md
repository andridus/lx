# LX Language - Metaprogramming Foundation

LX is a programming language designed with metaprogramming as its core feature. The `defmacro` system serves as the foundation for building all other language constructs.

## Quick Start

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd lx-lang/lx

# Install globally
make install

# Test the installation
lx-compiler run examples/macro_test.lx
```

### Basic Usage

```bash
# Compile and run LX files
lx-compiler run <filename.lx>

# Examples
lx-compiler run examples/macro_test.lx
lx-compiler run examples/macro_infix.lx
```

## Features

### ✅ Currently Implemented

- **Macro Definitions**: Define custom macros with `defmacro`
- **Block Syntax**: Use `do/end` blocks for multiple expressions
- **Basic Data Types**: Atoms, integers, floats, strings, lists, tuples, maps
- **Binary Operations**: Arithmetic and comparison operators
- **Global Installation**: Access from anywhere in the system

### 🔄 Coming Soon

- **Macro Calls**: Invoke defined macros
- **Macro Expansion**: Transform macro calls to AST
- **Infix Macros**: Custom operator definitions
- **Function System**: Function definition macros

## Examples

### Basic Macro Definition

```lx
defmacro a(body) do
  {:a, {1, 1, any, [{:args, []}]}, [body]}
end
```

### Block Macro with Multiple Expressions

```lx
defmacro b(body) do
  {:b, {1, 1, any, [{:args, []}]}, [body]}
end
```

### Do/End Blocks

```lx
do
  x = 1
  y = 2
  [x, y, 3]
end
```

## Build Commands

```bash
# Full build and installation
make

# Compile only
make compile

# Create executable
make escript

# Install globally
make install

# Uninstall
make uninstall

# Clean build artifacts
make clean

# Run tests
make test

# Test macro functionality
make run-macro-test
```

## Project Structure

```
lx/
├── src/                    # Source code
│   ├── lx_lexer.xrl       # Lexer with macro support
│   ├── lx_parser.yrl      # Parser with macro grammar
│   ├── lx_macros.erl      # Macro management system
│   ├── lx_compiler.erl    # Compiler integration
│   ├── lx_cli.erl         # Command-line interface
│   └── lx.erl             # Main module
├── examples/              # Example files
│   ├── macro_test.lx      # Basic macro test
│   └── macro_infix.lx     # Macro definitions
├── docs/                  # Documentation
│   └── IMPLEMENTATION_SUMMARY.md
└── Makefile              # Build scripts
```

## Development

### Prerequisites

- Erlang/OTP 24+
- rebar3

### Building from Source

```bash
# Install dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Create executable
rebar3 escriptize

# Test
rebar3 eunit
```

### Running Tests

```bash
# Run all tests
make test

# Run with verbose output
make test-verbose

# Run with coverage
make test-coverage
```

## Architecture

### Compilation Pipeline

1. **Lexical Analysis**: Tokenize source code
2. **Parsing**: Create initial AST
3. **Macro Expansion**: Process defined macros
4. **Output**: Return expanded AST

### AST Structure

```erlang
% Macro definition
{macro_def, Line, Name, Parameters, Body}

% Do/end block
{do_block, Line, ExpressionList}

% Binary operation
{binary_op, Line, Left, Operator, Right}
```

## Documentation

- [Implementation Summary](docs/IMPLEMENTATION_SUMMARY.md) - Detailed technical overview
- [Macro System Documentation](../docs/DEFMACRO_IMPLEMENTATION.md) - Macro system design

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## License

[Add your license information here]

## Status

**Current Phase**: Phase 1 Complete ✅
**Next Milestone**: Macro Call Implementation 🔄

The LX language is actively developed with a focus on providing a solid foundation for metaprogramming capabilities.