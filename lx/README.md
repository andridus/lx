# LX Language Compiler

A simple compiler for the LX language built with Erlang using Yecc and Leex.

## Features

- Lexical analysis with Leex
- Syntax analysis with Yecc
- Support for:
  - Integers and floats
  - Atoms
  - Tuples
  - Lists
  - Maps
  - Comments (lines starting with #)

## Building

```bash
make all
```

This will:
1. Compile the lexer and parser
2. Build the application
3. Create the executable `lx`

## Usage

```bash
./lx run examples/literals.lx
```

## Example

The file `examples/literals.lx` contains examples of all supported literals:

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

# maps
%{a: 1, b: 2}
%{"a": 1, "b": 2, 123: 1}
```

## Development

- `make compile` - Compile the project
- `make clean` - Clean build artifacts
- `make test` - Run tests
- `make run-example` - Run the example file