# LX Parser

The LX Parser is a comprehensive parsing system for the LX language, implemented in V. It provides a complete parsing pipeline from tokens to Abstract Syntax Trees (AST), supporting all LX language features as defined in the syntax reference.

## Architecture

The parser is organized into several specialized components:

### Core Parser (`core.v`)
- Base parser functionality with token management
- Error handling and position tracking
- Common parsing utilities

### Expression Parser (`expressions.v`)
- Parses all expression types in LX
- Handles operator precedence and associativity
- Supports literals, variables, function calls, and complex expressions

### Statement Parser (`statements.v`)
- Parses function definitions, record definitions, and other declarations
- Handles OTP components (workers, supervisors)
- Processes specifications and test blocks

### Main Parser (`main.v`)
- Orchestrates the entire parsing process
- Handles module-level constructs
- Manages imports and exports

## Features

### Expression Parsing

The expression parser supports all LX expression types:

#### Literals
```lx
42              // Integer
3.14            // Float
"hello"         // String
true            // Boolean
:ok             // Atom
nil             // Nil
```

#### Binary Operations
```lx
1 + 2 * 3       // Arithmetic with precedence
x == y          // Comparison
a andalso b     // Logical operations
"hello" ++ "world"  // String concatenation
```

#### Data Structures
```lx
[1, 2, 3]       // Lists
{1, "hello"}    // Tuples
%{name: "Alice", age: 30}  // Maps with atom keys
%{"key" => "value"}        // Maps with general keys
```

#### Control Flow
```lx
if x > 0 do
  "positive"
else
  "negative"
end

case value do
  1 -> "one"
  2 -> "two"
  _ -> "other"
end

for x in list when x > 0 do
  x * 2
end
```

#### Pattern Matching
```lx
%{name: user_name, age: user_age} <- user_data
[head | tail] <- list
{ok, result} <- computation()
```

#### Message Passing
```lx
pid ! {:hello, "world"}    // Send message
receive do
  :ready -> proceed()
  {:data, D} -> handle(D)
end after 5000 do
  :timeout
end
```

### Statement Parsing

The statement parser handles all LX declarations:

#### Function Definitions
```lx
def add(x, y) do
  x + y
end

def factorial do
  (0) do 1 end
  (N) when N > 0 do N * factorial(N - 1) end
end
```

#### Record Definitions
```lx
record Person {
  name :: string,
  age :: integer,
  email :: string
}
```

#### OTP Components
```lx
worker my_worker do
  def init(_) do {:ok, []} end
  def handle_call(:get, _from, state) do {:reply, state, state} end
end

supervisor top_sup {
  strategy one_for_one
  children [my_worker]
}
```

#### Specifications
```lx
spec divide {
  requires y != 0
  ensures result * y == x
}
```

#### Testing
```lx
describe "math tests" {
  test "adds two numbers" {
    assert 2 + 2 == 4
  }
}
```

### Module Parsing

The main parser handles complete LX modules:

```lx
module math_utils [add, multiply] {
  import stdlib

  def add(x, y) do
    x + y
  end

  def multiply(x, y) do
    x * y
  end
}
```

## Usage

### Basic Usage

```v
import parser
import lexer

// Create tokens (from lexer)
tokens := [...] // Your tokens here

// Parse expressions
mut expr_parser := parser.new_expression_parser(tokens)
expr := expr_parser.parse_expression() or { panic('Parse failed') }

// Parse statements
mut stmt_parser := parser.new_statement_parser(tokens)
stmt := stmt_parser.parse_statement() or { panic('Parse failed') }

// Parse complete modules
mut main_parser := parser.new_main_parser(tokens)
module := main_parser.parse_module() or { panic('Parse failed') }
```

### Error Handling

The parser provides comprehensive error handling:

```v
mut parser := parser.new_expression_parser(tokens)
expr := parser.parse_expression()

if parser.has_errors() {
  for error in parser.get_errors() {
    println('Error: ${error.message}')
  }
}
```

### AST Navigation

The parsed AST can be traversed and analyzed:

```v
match expr {
  ast.BinaryExpr {
    println('Binary operation: ${expr.op.str()}')
    println('Left: ${expr.left.str()}')
    println('Right: ${expr.right.str()}')
  }
  ast.CallExpr {
    println('Function call: ${expr.function.str()}')
    println('Arguments: ${expr.arguments.len}')
  }
  else {
    println('Other expression: ${expr.str()}')
  }
}
```

## Operator Precedence

The parser implements the following operator precedence (from lowest to highest):

1. `or`, `orelse`
2. `and`, `andalso`
3. `==`, `!=`, `<`, `>`, `<=`, `>=`
4. `++` (string concatenation)
5. `+`, `-`
6. `*`, `/`
7. `!` (send)
8. `::` (cons)
9. Unary `not`, `-`
10. Primary expressions (literals, variables, calls, etc.)

## Pattern Matching

The parser supports comprehensive pattern matching:

- **Variable patterns**: `x`, `_`
- **Literal patterns**: `1`, `"hello"`, `:ok`
- **Tuple patterns**: `{x, y}`, `{ok, result}`
- **List patterns**: `[head | tail]`, `[1, 2, 3]`
- **Map patterns**: `%{name: n, age: a}`, `%{"key" => value}`
- **Record patterns**: `Person{name: n, age: a}`

## Guards

Guards are supported in function clauses and case expressions:

```lx
def factorial do
  (0) do 1 end
  (N) when N > 0 do N * factorial(N - 1) end
end

case value do
  x when x > 0 -> "positive"
  x when x < 0 -> "negative"
  0 -> "zero"
end
```

## Testing

The parser includes comprehensive tests covering:

- Literal parsing
- Binary expression parsing with precedence
- Data structure parsing
- Function and record definitions
- Pattern matching
- Control flow expressions
- Message passing
- Module parsing
- Error handling

Run tests with:

```bash
v test lxc/tests/parser_test.v
v test lxc/tests/integration_test.v
```

## Error Recovery

The parser implements error recovery strategies:

- **Graceful degradation**: Continues parsing after encountering errors
- **Error collection**: Accumulates all errors for comprehensive reporting
- **Position tracking**: Provides accurate error locations
- **Contextual messages**: Offers helpful error messages with suggestions

## Performance

The parser is designed for efficiency:

- **Single-pass parsing**: Processes tokens in a single traversal
- **Minimal memory allocation**: Reuses structures where possible
- **Early error detection**: Fails fast on syntax errors
- **Optimized precedence handling**: Uses recursive descent for operator precedence

## Extensibility

The parser is designed to be easily extensible:

- **Modular architecture**: Each parser component is independent
- **Clear interfaces**: Well-defined boundaries between components
- **Comprehensive AST**: Rich representation for all language constructs
- **Error handling**: Extensible error reporting system

## Future Enhancements

Planned improvements include:

- **Incremental parsing**: Support for parsing partial code
- **Syntax highlighting**: Integration with editor support
- **Code formatting**: Pretty-printing of parsed AST
- **Performance profiling**: Detailed performance analysis
- **Language server**: Integration with language server protocol