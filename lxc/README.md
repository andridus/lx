# LX Compiler Foundation

This directory contains the foundation implementation of the LX compiler written in V. The foundation provides the basic building blocks for the complete compiler pipeline.

## Project Structure

```
lxc/
├── v.mod                 # V project configuration
├── main.v                # CLI entry point
├── Makefile              # Build and test automation
├── ast/                  # Abstract Syntax Tree definitions
│   ├── position.v        # Source position and span tracking
│   ├── types.v           # LX type system and literals
│   └── ast.v             # Main AST structures
├── error/                # Error handling system
│   ├── error.v           # Error types and collection
│   ├── formatter.v       # Error formatting and display
│   └── suggestions.v     # Error correction suggestions
├── utils/                # Utility modules
│   ├── string.v          # String manipulation utilities
│   ├── file.v            # File I/O utilities
│   └── debug.v           # Debugging and logging utilities
└── tests/                # Test suite
    └── test_foundation.v # Comprehensive foundation tests
```

## Features Implemented

### T1.1: Project Setup ✅
- **V Project Configuration**: Complete `v.mod` with project metadata
- **CLI Interface**: Command-line interface with help, version, build, and file compilation options
- **Build System**: Makefile with build, test, clean, and development targets
- **Project Structure**: Modular organization following V best practices

### T1.2: AST Definitions ✅
- **Position Tracking**: Line, column, and filename tracking for precise error reporting
- **Type System**: Complete LX type system including basic types, collections, and special types
- **Literals**: Support for all LX literal types (string, integer, float, boolean, atom, nil)
- **Expressions**: Comprehensive expression AST including variables, assignments, binary operations, function calls, pattern matching, and more
- **Patterns**: Pattern matching structures for variables, literals, atoms, lists, tuples, maps, records, and binaries
- **Statements**: Module definitions, function definitions, record definitions, and type definitions

### T1.3: Error System ✅
- **Error Types**: Comprehensive error classification (lexical, syntax, type, semantic, unbound variable, undefined function, pattern, module, record, guard, internal)
- **Error Collection**: Centralized error collection with severity levels (info, warning, error, fatal)
- **Error Formatting**: Rich error formatting with source context, colored output, and error indicators
- **Error Suggestions**: Intelligent error correction suggestions using Levenshtein distance and common patterns

### T1.4: Utilities ✅
- **String Utilities**: Escape/unescape functions, identifier validation, atom validation, module name validation, record name validation, whitespace normalization, identifier splitting
- **String Pooling**: Memory optimization through string interning
- **File Utilities**: File I/O operations, file information, directory operations, temporary file management, LX file discovery
- **Debug Utilities**: Debug logging with levels, performance timing, performance tracking, debug context

## Usage

### Building the Project

```bash
# Build the compiler
make build

# Run tests
make test

# Clean build artifacts
make clean

# Format code
make fmt

# Check code
make check
```

### Using the CLI

```bash
# Show help
./lx_compiler --help

# Show version
./lx_compiler --version

# Build the project
./lx_compiler --build

# Compile a file
./lx_compiler --file example.lx

# Compile with output specification
./lx_compiler --file example.lx --output example.erl
```

### Running Tests

```bash
# Run all tests
v test .

# Run specific test file
v test tests/test_foundation.v

# Run tests with Makefile
make test
```

## AST Examples

### Basic Expressions

```v
// Variable reference
var_expr := ast.Expr.var('x')

// Literal
literal_expr := ast.Expr.literal(types.Literal.l_integer(42))

// Assignment
assign_expr := ast.Expr.assign{
    name: 'x'
    value: ast.Expr.literal(types.Literal.l_integer(5))
    position: position.new_position(1, 1, 'test.lx')
}

// Binary operation
binary_expr := ast.Expr.binary{
    left: ast.Expr.var('x')
    op: ast.BinaryOp.add
    right: ast.Expr.literal(types.Literal.l_integer(10))
    position: position.new_position(1, 5, 'test.lx')
}
```

### Patterns

```v
// Wildcard pattern
wildcard := ast.Pattern.p_wildcard

// Variable pattern
var_pattern := ast.Pattern.p_var('x')

// Atom pattern
atom_pattern := ast.Pattern.p_atom('ok')

// Literal pattern
literal_pattern := ast.Pattern.p_literal(types.Literal.l_integer(42))

// List pattern
list_pattern := ast.Pattern.p_list_cons{
    head: ast.Pattern.p_var('head')
    tail: ast.Pattern.p_var('tail')
}
```

### Error Handling

```v
// Create an error
error := error.new_compilation_error(
    error.ErrorKind.syntax_error{
        message: 'Unexpected token'
        expected: 'identifier'
        found: '='
    },
    position.new_position(1, 5, 'test.lx'),
    'Expected identifier, found ='
)

// Add to collection
mut collection := error.new_error_collection()
collection.add_error(error)

// Format error
formatter := formatter.new_error_formatter()
formatted := formatter.format_error(error, source_lines)
```

### String Utilities

```v
// Validate identifiers
assert string.is_valid_identifier('valid_var') == true
assert string.is_valid_identifier('123invalid') == false

// Validate atoms
assert string.is_valid_atom('ok') == true
assert string.is_valid_atom('error') == true

// Validate module names
assert string.is_valid_module_name('Module') == true
assert string.is_valid_module_name('MyModule') == true

// String pooling for memory optimization
mut pool := string.new_string_pool()
str1 := pool.intern('hello')
str2 := pool.intern('hello')
assert str1 == str2  // Same reference
```

### Debug Utilities

```v
// Create debug logger
mut logger := debug.new_debug_logger(debug.DebugLevel.info)

// Log messages
logger.info('Processing file', 'lexer')
logger.debug('Token found', 'identifier')
logger.error('Syntax error', 'unexpected token')

// Performance timing
mut timer := debug.new_performance_timer('lexer')
timer.start()
// ... do work ...
timer.stop()
println('Lexer took: ${timer.get_duration()}ms')
```

## Type System

The LX type system includes:

### Basic Types
- `integer` - 64-bit integers
- `float` - 64-bit floating point
- `string` - UTF-8 strings
- `boolean` - true/false values
- `atom` - Symbolic atoms (like Erlang)
- `nil` - Null value

### Collection Types
- `list` - Linked lists
- `tuple` - Fixed-size tuples
- `map` - Key-value maps
- `record` - Named field structures

### Special Types
- `function` - Function references
- `pid` - Process identifiers
- `reference` - Unique references
- `port` - Port references
- `binary` - Binary data
- `bitstring` - Bit-level data

### Meta Types
- `any` - Any type
- `unknown` - Unknown type

## Error System Features

### Error Types
- **Lexical Errors**: Invalid tokens, unterminated strings, etc.
- **Syntax Errors**: Unexpected tokens, missing delimiters, etc.
- **Type Errors**: Type mismatches, invalid operations, etc.
- **Semantic Errors**: Logic errors, invalid semantics, etc.
- **Unbound Variable Errors**: Undefined variables with suggestions
- **Undefined Function Errors**: Missing function calls with suggestions
- **Pattern Errors**: Invalid pattern matching
- **Module Errors**: Import/export issues
- **Record Errors**: Missing fields, invalid access
- **Guard Errors**: Invalid guard expressions
- **Internal Errors**: Compiler internal errors

### Error Formatting
- **Colored Output**: ANSI color codes for different severity levels
- **Source Context**: Shows relevant source code lines around errors
- **Error Indicators**: Visual indicators showing exact error positions
- **Suggestions**: Helpful hints for fixing common errors

### Error Suggestions
- **Syntax Suggestions**: Common syntax fixes (:= vs =, semicolons, etc.)
- **Type Suggestions**: Type conversion hints
- **Variable Suggestions**: Similar variable name suggestions
- **Function Suggestions**: Similar function name suggestions
- **Pattern Suggestions**: Pattern matching improvements

## Performance Features

### String Pooling
- **Memory Optimization**: Reduces memory usage for repeated strings
- **Reference Equality**: Fast string comparison using reference equality
- **Automatic Management**: Automatic cleanup and management

### Performance Tracking
- **Timers**: Individual performance timers for components
- **Trackers**: Centralized performance tracking
- **Summaries**: Performance summaries and reports

## Testing

The test suite covers:

- **AST Structures**: All AST node types and operations
- **Error System**: Error creation, collection, formatting, and suggestions
- **String Utilities**: All string manipulation functions
- **File Utilities**: File I/O operations and management
- **Debug Utilities**: Logging, timing, and debugging features
- **Type System**: All LX types and literals
- **Edge Cases**: Boundary conditions and error cases

## Next Steps

The foundation provides a solid base for implementing:

1. **Lexer**: Token generation from source code
2. **Parser**: AST construction from tokens
3. **Type Checker**: Type analysis and validation
4. **Code Generator**: Erlang code generation
5. **Optimizer**: Code optimization passes

## Contributing

When contributing to the foundation:

1. **Follow V Conventions**: Use V naming conventions and patterns
2. **Add Tests**: Ensure all new features have comprehensive tests
3. **Update Documentation**: Keep documentation current with changes
4. **Error Handling**: Use the error system for all error reporting
5. **Performance**: Consider performance implications of changes

## License

This project is licensed under the MIT License.