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
lx examples/literals.lx
```

### Basic Usage

```bash
# Run LX files directly (default mode)
lx <filename.lx>

# Compile to .erl file
lx compile <filename.lx>

# Show AST (Abstract Syntax Tree)
lx ast <filename.lx>

# Examples
lx examples/literals.lx
lx compile examples/literals.lx
lx ast examples/literals.lx
```

## Features

### ✅ Currently Implemented

- **Multiple Output Modes**: Run directly, compile to .erl, show AST
- **Direct File Execution**: Run `.lx` files directly without additional commands
- **Global Installation**: Access from anywhere in the system
- **AST Generation**: Complete Abstract Syntax Tree generation
- **Code Generation**: Generate Erlang source code (.erl files)
- **BEAM Compilation**: Direct compilation to BEAM bytecode
- **Runtime Execution**: Execute compiled AST with value evaluation
- **Basic Data Types**: Atoms, integers, floats, strings, lists, tuples, maps
- **Binary Operations**: Arithmetic operators (+, -, *, /)
- **Comments**: Support for `#` comments
- **Macro Definitions**: Define custom macros with `defmacro` (parsing only)

### 🔄 Coming Soon

- **Macro Calls**: Invoke defined macros
- **Macro Expansion**: Transform macro calls to AST
- **Infix Macros**: Custom operator definitions
- **Function System**: Function definition macros

## Examples

### Basic Literals

```lx
#integer
1

#float
2.0

#atoms
:atom
:ok

#tuples
{1}
{1,2}
{:ok, 1,2,3}

#lists
[1]
[1,2]
[1,2,3, :ok]

#maps
%{a: 1, b: 2}
%{"a": 1, "b": 2, 123: 1}
```

### Binary Operations

```lx
1 + 2
3 * 4
10 - 5
8 / 2
```

### Macro Definitions

```lx
defmacro a(body) do
  {:a, {1, 1, any, [{:args, []}]}, [body]}
end

defmacro b(body) do
  {:b, {1, 1, any, [{:args, []}]}, [body]}
end
```

## Build Commands

```bash
# Full build and installation
make

# Compile only
make compile

# Install globally
make install

# Uninstall
make uninstall

# Clean build artifacts
make clean

# Run tests
make test

# Test examples
make run-example
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
│   ├── lx_codegen.erl     # Code generation (NEW)
│   ├── lx_cli.erl         # Command-line interface
│   └── lx.erl             # Main module
├── examples/              # Example files
│   ├── literals.lx        # Basic literals test
│   ├── simple.lx          # Simple examples
│   └── macro_test.lx      # Macro definitions
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
4. **Code Generation**: Generate Erlang code or BEAM

### AST Structure

```erlang
% Literals
{integer, Line, Value}
{float, Line, Value}
{atom, Line, Value}
{string, Line, Value}

% Data structures
{tuple, Line, Elements}
{list, Line, Elements}
{map, Line, Entries}

% Binary operations
{binary_op, Line, Left, Operator, Right}

% Macro definitions
{macro_def, Line, Name, Parameters, Body}
{macro_def_infix, Line, Name, Parameters, Body}
```

## CLI Interface

### Multiple Output Modes

```bash
# Execute a .lx file directly (default)
lx filename.lx

# Compile to .erl file
lx compile filename.lx

# Show AST structure
lx ast filename.lx

# Execute with arguments (future feature)
lx filename.lx arg1 arg2
```

### Error Handling

- **File not found**: Clear error message
- **Compilation errors**: Line numbers and context
- **Runtime errors**: Expression evaluation errors
- **Empty files**: Graceful handling

## Code Generation

### Multiple Output Formats

The LX compiler supports multiple output formats:

1. **Direct Execution (Default)**: Compile to BEAM and execute immediately
2. **Erlang Source (.erl)**: Generate human-readable Erlang source code
3. **AST Display**: Show the Abstract Syntax Tree structure
4. **BEAM Only**: Compile directly to BEAM bytecode

### Example Output

#### Successful Execution:
```bash
$ lx examples/literals.lx
#{123 => {tuple,1,2,3},
  list => [1,2,3,4],
  map => #{a => 1,b => 2},
  <<"a">> => 1,<<"b">> => 2}
```

#### Generated .erl File:
```erlang
-module(literals).
-export([main/0]).

main() ->
    1,
    2.0,
    atom,
    ok,
    {1},
    {1, 2},
    {ok, 1, 2, 3},
    [1],
    [1, 2],
    [1, 2, 3, ok],
    #{a => 1, b => 2},
    #{"a" => 1, "b" => 2, 123 => 1},
    #{"a" => 1, "b" => 2, 123 => {tuple, 1, 2, 3},
      list => [1, 2, 3, 4],
      map => #{a => 1, b => 2}}.
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

### Recent Updates

- ✅ **Multiple Output Modes**: Run, compile, and AST display
- ✅ **Code Generation**: Generate .erl files and BEAM bytecode
- ✅ **Direct Execution**: Execute LX files without intermediate steps
- ✅ **Global Installation**: Install LX globally for system-wide access
- ✅ **Improved CLI**: Better error handling and user experience