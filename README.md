# Lx Programming Language

Lx is a functional programming language designed for building robust OTP (Open Telecom Platform) applications with seamless Erlang/BEAM interoperability. It combines the safety and expressiveness of functional programming with the battle-tested concurrency model of the Erlang ecosystem.

## Features

### Core Language Features
- **Functional Programming**: Immutable data structures, pattern matching, and higher-order functions
- **Type Safety**: Comprehensive static analysis and linting system
- **OTP Integration**: First-class support for workers, supervisors, and OTP applications
- **Erlang Interoperability**: Seamless integration with existing Erlang/OTP codebases
- **Modern Syntax**: Clean, readable syntax inspired by modern functional languages

### Development Experience
- **Comprehensive Linting**: Static analysis catches errors before runtime
- **Automatic Build System**: Integrated rebar3 compilation with dependency management
- **Clear Error Messages**: Precise error reporting with helpful suggestions
- **Testing Framework**: Built-in testing support with descriptive test suites

## Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/your-username/lx-lang.git
cd lx-lang

# Build the compiler
make build

# Install (optional)
make install
```

### Your First Lx Program

Create a file called `hello.lx`:

```lx
application {
  description "Hello World Application"
  vsn "1.0.0"
}

worker greeter {
  fun init(_) {
    .{:ok, "Hello, World!"}
  }

  fun handle_call(:greet, _from, message) {
    .{:reply, message, message}
  }

  pub fun greet() {
    gen_server.call(__MODULE__, :greet)
  }
}

supervisor main_supervisor {
  strategy one_for_one
  children [greeter]
}
```

Compile and run:

```bash
# Compile the application
lx hello.lx

# The compiler automatically generates a complete OTP application
# Navigate to the generated project
cd _build/hello

# Start the application
rebar3 shell
```

## Language Overview

### Variables and Expressions

```lx
fun calculate_total(items) {
  # Variable assignment
  subtotal = sum_prices(items)
  tax_rate = 0.08
  tax = subtotal * tax_rate

  # Block expressions
  total = {
    base = subtotal + tax
    rounded = round(base, 2)
    rounded
  }

  total
}
```

### Pattern Matching

```lx
fun process_result(result) {
  case result {
    .{:ok, data} -> {
      processed = transform_data(data)
      .{:success, processed}
    }
    .{:error, reason} -> {
      log_error(reason)
      .{:failure, reason}
    }
    _ -> .{:unknown, result}
  }
}
```

### OTP Workers

```lx
worker user_manager {
  fun init(_) {
    .{:ok, []}
  }

  fun handle_call(.{:add_user, user}, _from, users) {
    new_users = [user | users]
    .{:reply, :ok, new_users}
  }

  fun handle_call(:get_users, _from, users) {
    .{:reply, users, users}
  }

  # Public API functions
  pub fun add_user(user) {
    gen_server.call(__MODULE__, .{:add_user, user})
  }

  pub fun get_users() {
    gen_server.call(__MODULE__, :get_users)
  }
}
```

### Testing

```lx
describe "user manager tests" {
  test "should add and retrieve users" {
    # Setup
    users = ["alice", "bob"]

    # Test adding users
    for user in users {
      result = user_manager.add_user(user)
      assert result == :ok
    }

    # Test retrieving users
    retrieved = user_manager.get_users()
    assert length(retrieved) == 2
  }
}
```

## Compilation

### Basic Compilation

```bash
# Compile a single file
lx myapp.lx

# Type check only
lx --type-check myapp.lx

# Skip rebar3 compilation (faster for development)
lx --skip-rebar myapp.lx
```

### Build Flags

- `--type-check`: Perform type checking without code generation
- `--skip-rebar`: Generate Erlang code without running rebar3 compilation
- `--help`: Show all available options

## Project Structure

```
lx-lang/
├── lx/                    # Compiler source code
│   └── compiler/          # Core compiler modules
├── docs/                  # Documentation
│   ├── Lx_SYNTAX_REFERENCE.md
│   └── Lx_DEV_CHANGELOG.md
├── examples/              # Example Lx programs
├── tests/                 # Compiler test suite
├── _build/                # Build artifacts
├── dune-project           # Dune build configuration
├── lx.opam               # OCaml package definition
└── Makefile              # Build automation
```

## Documentation

- **[Syntax Reference](docs/Lx_SYNTAX_REFERENCE.md)**: Complete language syntax guide
- **[Development Changelog](docs/Lx_DEV_CHANGELOG.md)**: Recent improvements and features
- **[Test Suite](tests/README.md)**: Information about the test suite

## Examples

The `examples/` directory contains various Lx programs demonstrating different features:

- **Basic Applications**: Simple OTP applications
- **Worker Patterns**: Different worker implementations
- **Supervisor Trees**: Complex supervision hierarchies
- **Testing Examples**: Comprehensive test suites

## Development

### Building from Source

```bash
# Install dependencies
opam install dune alcotest

# Build the compiler
dune build

# Run tests
dune runtest

# Install locally
dune install
```

### Testing

```bash
# Run all tests
make test

# Run specific test suites
dune exec tests/test_main.exe

# Run with verbose output
dune runtest --verbose
```

## Architecture

The Lx compiler is built with OCaml and follows a traditional compiler architecture:

1. **Lexical Analysis**: Tokenizes Lx source code
2. **Parsing**: Builds Abstract Syntax Trees (AST) using Menhir
3. **Static Analysis**: Comprehensive linting and type checking
4. **Code Generation**: Transforms AST to Erlang code
5. **Build Integration**: Automatic rebar3 compilation and project generation

### Key Components

- **Lexer**: OCamllex-based tokenizer
- **Parser**: Menhir-generated parser with comprehensive grammar
- **Linter**: Static analysis engine for code quality
- **Code Generator**: AST-to-Erlang transformation
- **Build System**: Integrated rebar3 workflow

## Contributing

We welcome contributions! Please see our contributing guidelines:

1. **Fork** the repository
2. **Create** a feature branch
3. **Add** tests for new functionality
4. **Ensure** all tests pass
5. **Submit** a pull request

### Development Setup

```bash
# Clone your fork
git clone https://github.com/your-username/lx-lang.git
cd lx-lang

# Install development dependencies
opam install --deps-only .

# Build and test
make build
make test
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Community

- **Issues**: Report bugs and request features on GitHub Issues
- **Discussions**: Join conversations on GitHub Discussions
- **Documentation**: Contribute to docs and examples

## Roadmap

- **Module System**: Import/export system for multi-module projects
- **Package Manager**: Native package manager for Lx libraries
- **IDE Integration**: Language server protocol support
- **Advanced Types**: More sophisticated type system features
- **Concurrency Primitives**: Native support for OTP concurrency patterns

---

**Lx** - *Functional. Concurrent. Reliable.*