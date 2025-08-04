# LX Language Compiler - Usage Guide

## Building the Compiler

```bash
make all
```

This command will:
1. Compile the lexer (lx_lexer.xrl) and parser (lx_parser.yrl)
2. Build the Erlang application
3. Create the executable `lx`

## Running the Compiler

```bash
./lx run <filename>
```

Example:
```bash
./lx run examples/literals.lx
```

## Supported Syntax

The LX compiler supports the following literals:

### Integers
```lx
1
42
-10
```

### Floats
```lx
2.0
3.14
-1.5
```

### Atoms
```lx
:atom
:ok
:error
```

### Tuples
```lx
{1}
{1, 2}
{:ok, 1, 2, 3}
```

### Lists
```lx
[1]
[1, 2]
[1, 2, 3, :ok]
```

### Maps
```lx
%{a: 1, b: 2}
%{"a": 1, "b": 2, 123: 1}
```

### Comments
```lx
# This is a comment
# Comments start with #
```

## Output

The compiler generates an Abstract Syntax Tree (AST) in Erlang format. Each node contains:
- Type (integer, float, atom, tuple, list, map)
- Line number
- Value or child nodes

## Example Output

For the file `examples/literals.lx`:

```
Compilation successful!
AST: [{integer,2,1},
      {float,5,2.0},
      {atom,8,atom},
      {atom,10,ok},
      {tuple,13,[{integer,13,1}]},
      {tuple,15,[{integer,15,1},{integer,15,2}]},
      {tuple,17,[{atom,17,ok},{integer,17,1},{integer,17,2},{integer,17,3}]},
      {list,21,[{integer,21,1}]},
      {list,23,[{integer,23,1},{integer,23,2}]},
      {list,25,[{integer,25,1},{integer,25,2},{integer,25,3},{atom,25,ok}]},
      {map,29,
           [{map_entry,29,{atom,29,a},{integer,29,1}},
            {map_entry,29,{atom,29,b},{integer,29,2}}]},
      {map,30,
           [{map_entry,30,{string,30,"a"},{integer,30,1}},
            {map_entry,30,{string,30,"b"},{integer,30,2}},
            {map_entry,30,{integer,30,123},{integer,30,1}}]}]
```

## Development Commands

- `make compile` - Compile the project
- `make clean` - Clean build artifacts
- `make test` - Run tests
- `make run-example` - Run the example file