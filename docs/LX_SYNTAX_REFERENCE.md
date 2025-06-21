# LX Language Syntax Reference

This document provides a comprehensive reference for the LX programming language syntax. LX is a functional language designed for building OTP (Open Telecom Platform) applications with Erlang/BEAM interoperability.

## Table of Contents

1. [Lexical Elements](#lexical-elements)
2. [Keywords](#keywords)
3. [Operators and Punctuation](#operators-and-punctuation)
4. [Literals](#literals)
5. [Identifiers](#identifiers)
6. [Comments](#comments)
7. [Expressions](#expressions)
8. [Function Definitions](#function-definitions)
9. [Pattern Matching](#pattern-matching)
10. [OTP Components](#otp-components)
11. [Specifications](#specifications)
12. [Testing](#testing)
13. [Examples](#examples)

## Lexical Elements

### Keywords

LX has several categories of keywords:

#### Core Language Keywords
- `fun` - Function definition
- `case` - Pattern matching
- `if` - Conditional expression
- `then` - Used with `if` expressions
- `else` - Used with `if` expressions
- `for` - Loop iteration
- `when` - Guard expressions
- `true` - Boolean literal
- `false` - Boolean literal
- `nil` - Null/empty value


#### OTP Keywords
- `worker` - Defines an OTP worker (gen_server)
- `supervisor` - Defines an OTP supervisor
- `strategy` - Supervisor restart strategy
- `children` - Supervisor child processes
- `one_for_one` - Supervisor strategy
- `one_for_all` - Supervisor strategy
- `rest_for_one` - Supervisor strategy

#### Specification Keywords
- `spec` - Formal specification
- `requires` - Preconditions
- `ensures` - Postconditions
- `matches` - Pattern matching constraints

#### Testing Keywords
- `describe` - Test suite description
- `test` - Individual test case
- `assert` - Test assertion

## Operators and Punctuation

### Operators
- `=` - Assignment/binding
- `->` - Function arrow, case branch
- `|` - Pipe operator
- `_` - Wildcard pattern
- `::` - List cons operator
- `.` - Module access
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division

### Punctuation
- `(` `)` - Parentheses (grouping, function calls)
- `{` `}` - Braces (blocks, function bodies)
- `.{` `}` - Dot-braces (tuples)
- `[` `]` - Brackets (lists)
- `,` - Comma (separators)
- `;` - Semicolon (expression sequences)

## Literals

### String Literals
```lx
"hello world"
"escaped \"quotes\""
"multiline\nstring"
```

Supported escape sequences:
- `\"` - Double quote
- `\\` - Backslash
- `\n` - Newline
- `\t` - Tab
- `\r` - Carriage return

### Integer Literals
```lx
42
0
-17
```

### Float Literals
```lx
3.14
0.0
-2.5
```

### Boolean Literals
```lx
true
false
```

### Atom Literals
```lx
:ok
:error
:timeout
:my_atom
```

### Nil Literal
```lx
nil
```

## Identifiers

### Variable Identifiers
- Start with lowercase letter
- Can contain letters, digits, and underscores
- Examples: `x`, `my_var`, `count_1`

### Module/Constructor Identifiers
- Start with lowercase letter
- Can contain letters, digits, and underscores
- Examples: `my_module`, `result`, `http_client`

## Comments

Single-line comments start with `#`:
```lx
# This is a comment
x = 42  # End-of-line comment

# Comments can be at the end of file without newline
```

**Note**: Comments use `#` (hash/pound) symbol, not `//` like in C-style languages.

## Expressions

### Variable Assignment
LX supports direct variable assignment without explicit scoping keywords:

```lx
x = 42
y = "hello"
z = true
```

**Important**: Variables cannot be reassigned within the same scope. This will cause a compile-time error:
```lx
x = 42
x = 43  # Error: Variable 'x' is already defined and cannot be reassigned
```



### Block Expressions
Block expressions allow grouping multiple statements and return the value of the last expression. When compiled to Erlang, blocks are expanded inline for better performance:

```lx
# Simple block
result = {
  x = 10
  y = 20
  x + y  # This value is returned
}

# Nested blocks
complex_result = {
  a = {
    inner = 5
    inner * 2
  }
  b = 15
  a + b
}
```

**Compilation Behavior**: Block expressions are compiled as inline statements rather than anonymous functions, resulting in more efficient Erlang code:

```erlang
% Compiled output example:
% start block Result_abc
X_abc = 10,
Y_abc = 20,
% end block Result_abc
Result_def = X_abc + Y_abc
```

### Sequence Expressions
Function bodies can contain multiple expressions separated by newlines or semicolons:

```lx
fun process_data(input) {
  cleaned = clean_input(input)
  validated = validate_data(cleaned)
  transformed = transform_data(validated)
  transformed
}

# With semicolons (optional)
fun process_data_compact(input) {
  cleaned = clean_input(input);
  validated = validate_data(cleaned);
  transformed = transform_data(validated);
  transformed
}
```

### Function Application
```lx
func(arg1, arg2, arg3)
```

### External Function Calls
```lx
# Call function from another module
my_module.function_name(arg1, arg2)
```

### Conditional Expressions
```lx
if condition {
  true_branch
} else {
  false_branch
}

# Without else clause
if condition {
  expression
}
```

### Case Expressions (Pattern Matching)
```lx
case value {
  pattern1 -> expression1
  pattern2 -> expression2
  _ -> default_expression
}
```

### Arithmetic Expressions
```lx
# Basic arithmetic operations
x = 10 + 5    # Addition: 15
y = 20 - 8    # Subtraction: 12
z = 6 * 7     # Multiplication: 42
w = 15 / 3    # Division: 5

# Complex expressions with precedence
result = 2 + 3 * 4    # Result: 14 (multiplication has higher precedence)
total = (10 + 5) * 2  # Result: 30 (parentheses override precedence)
```

### For Loops
```lx
for item in list {
  process(item)
}
```

### Tuples
```lx
.{}             # Empty tuple
.{x}            # Single element tuple
.{x, y}         # Two elements
.{x, y, z}      # Three elements
```

### Lists
```lx
[]              # Empty list
[1, 2, 3]       # List with elements
[head | tail]   # Cons pattern
```

## Function Definitions

### Single Clause Functions
```lx
fun add(x, y) {
  x + y
}

# With multiple statements
fun complex_calculation(a, b) {
  temp1 = a * 2
  temp2 = b + 5
  result = temp1 + temp2
  result
}
```

### Multiple Clause Functions (Pattern Matching)
```lx
fun factorial {
  (0) { 1 }
  (n) { n * factorial(n - 1) }
}

fun process_result {
  (.{:ok, value}) { value }
  (.{:error, reason}) {
    log_error(reason)
    nil
  }
}

# Pattern matching with literals
fun describe_number {
  (0) { "zero" }
  (1) { "one" }
  (n) { "other number: " + string_of_int(n) }
}

# Pattern matching with multiple parameters
fun compare {
  (x, y) when x > y { "first is greater" }
  (x, y) when x < y { "second is greater" }
  (_, _) { "equal" }
}
```

### Empty Function Body
```lx
fun empty_func() {}
```

### Functions with Block Bodies
```lx
fun complex_function(input) {
  # Validate input
  if input == nil {
    .{:error, "Input cannot be nil"}
  } else {
    # Process the input
    processed = process_input(input)
    validated = validate_processed(processed)

    # Return result
    case validated {
      .{:ok, result} -> .{:ok, result}
      .{:error, reason} -> .{:error, reason}
    }
  }
}
```

## Pattern Matching

### Basic Patterns
```lx
case value {
  42 -> "forty-two"           # Literal pattern
  x -> "variable binding"     # Variable pattern
  _ -> "wildcard"             # Wildcard pattern
}
```

### Atom Patterns
```lx
case result {
  :ok -> "success"
  :error -> "failure"
  :timeout -> "timeout"
}
```

### Tuple Patterns
```lx
case tuple {
  .{x, y} -> x + y
  .{x, y, z} -> x + y + z
  .{} -> 0
}
```

### List Patterns
```lx
case list {
  [] -> "empty"
  [x] -> "single element"
  [x, y] -> "two elements"
  [head | tail] -> "cons pattern"
}
```

## OTP Components

### Worker Definition
```lx
worker my_worker {
  # Required init function
  fun init(args) {
    initial_state = setup_state(args)
    .{:ok, initial_state}
  }

  # Optional callback functions
  fun handle_call(request, from, state) {
    response = process_request(request, state)
    new_state = update_state(state, request)
    .{:reply, response, new_state}
  }

  fun handle_cast(request, state) {
    new_state = handle_async_request(request, state)
    .{:noreply, new_state}
  }

  fun handle_info(info, state) {
    .{:noreply, state}
  }

  fun terminate(reason, state) {
    cleanup_resources(state)
    .{:ok}
  }

  fun code_change(old_vsn, state, extra) {
    migrated_state = migrate_state(old_vsn, state, extra)
    .{:ok, migrated_state}
  }

  fun format_status(status) {
    formatted = format_for_debugging(status)
    formatted
  }

  # Regular helper functions
  fun helper_function(x, y) {
    result = x + y
    result
  }

  # Specifications
  spec helper_function {
    requires x > 0, y > 0
    ensures result > 0
  }
}
```

### OTP Callback Functions

#### Required Callbacks
- `init(Args)` - Initialize worker state
  - **Parameters**: 1 (initialization arguments)
  - **Return**: Must return a tuple

#### Optional Callbacks
- `handle_call(Request, From, State)` - Handle synchronous calls
  - **Parameters**: 3 (request, caller, current state)
  - **Return**: Must return a tuple (typically `.{:reply, Response, NewState}`)

- `handle_cast(Request, State)` - Handle asynchronous casts
  - **Parameters**: 2 (request, current state)
  - **Return**: Must return a tuple (typically `.{:noreply, NewState}`)

- `handle_info(Info, State)` - Handle system messages
  - **Parameters**: 2 (info message, current state)
  - **Return**: Must return a tuple

- `terminate(Reason, State)` - Cleanup on termination
  - **Parameters**: 2 (termination reason, current state)
  - **Return**: Must return a tuple

- `code_change(OldVsn, State, Extra)` - Handle code upgrades
  - **Parameters**: 3 (old version, current state, extra data)
  - **Return**: Must return a tuple

- `format_status(Status)` - Format status for debugging
  - **Parameters**: 1 (status information)
  - **Return**: Can return any type (not restricted to tuples)

### Supervisor Definition
```lx
supervisor my_supervisor {
  strategy one_for_one
  children [worker1, worker2, worker3]
}
```

#### Supervisor Strategies
- `one_for_one` - Restart only the failed child
- `one_for_all` - Restart all children when one fails
- `rest_for_one` - Restart the failed child and all children started after it

## Specifications

### Basic Specification
```lx
spec function_name {
  requires condition1, condition2
  ensures result_condition1, result_condition2
}
```

### Empty Specification
```lx
spec function_name {
}
```

## Testing

### Test Suites
```lx
describe "test suite description" {
  test "individual test description" {
    result = some_function(input)
    assert result == expected
  }

  test "another test" {
    # Multiple statements in test
    x = 10
    y = 20
    sum = x + y
    assert sum == 30
  }
}
```

### Standalone Tests
```lx
test "standalone test" {
  assert 1 + 1 == 2
}
```

### Test Expressions
Tests can contain any valid LX expression including assignments and blocks:
```lx
test "complex test with assignments" {
  input = "test_data"
  processed = {
    cleaned = clean_data(input)
    validated = validate_data(cleaned)
    validated
  }

  result = case processed {
    .{:ok, data} -> :success
    .{:error, _} -> :failure
  }

  assert result == :success
}
```

## Grammar Rules

### Program Structure
```
program ::= module_item* EOF

module_item ::= function_def
              | otp_component
              | spec_def
              | test_block
              | standalone_test

function_def ::= 'fun' IDENT '{' function_clause+ '}'           # Multiple clauses
               | 'fun' IDENT '(' params ')' '{' function_body '}'  # Single clause

function_clause ::= '(' params ')' '{' function_body '}'

function_body ::= expr
                | expr ';' function_body
                | statement_list

statement_list ::= statement
                 | statement statement_list

statement ::= expr
```

### Expression Grammar
```
expr ::= arithmetic_expr
       | IDENT '=' expr                                    # Assignment
       | expr '(' expr_list ')'                           # Function call
       | 'if' expr 'then' expr ('else' expr)?             # Conditional
       | 'case' expr '{' case_branch* '}'                 # Pattern matching
       | 'for' IDENT 'in' expr '{' expr '}'               # For loop

arithmetic_expr ::= term
                  | arithmetic_expr '+' term               # Addition
                  | arithmetic_expr '-' term               # Subtraction

term ::= factor
       | term '*' factor                                   # Multiplication
       | term '/' factor                                   # Division

factor ::= simple_expr
         | '(' arithmetic_expr ')'                         # Grouping

simple_expr ::= literal
              | IDENT                                      # Variable
              | IDENT '.' IDENT '(' expr_list ')'         # External call
              | IDENT '.' IDENT                           # Module reference
              | '(' expr ')'                              # Grouping
              | '.{' expr_list '}'                        # Tuple
              | '[' expr_list ']'                         # List
              | '{' statement_list '}'                    # Block expression
              | '{' '}'                                   # Empty block

case_branch ::= pattern '->' expr

pattern ::= simple_pattern
          | pattern '::' pattern                          # Cons pattern

simple_pattern ::= '_'                                    # Wildcard
                 | IDENT                                  # Variable
                 | ATOM                                   # Atom
                 | literal                                # Literal
                 | '.{' pattern_list '}'                  # Tuple pattern
                 | '[' pattern_list ']'                   # List pattern
```

## Reserved Words

The following words are reserved and cannot be used as identifiers:

**Core**: `fun`, `case`, `if`, `then`, `else`, `for`, `when`, `true`, `false`, `nil`

**OTP**: `worker`, `supervisor`, `strategy`, `children`, `one_for_one`, `one_for_all`, `rest_for_one`

**Specs**: `spec`, `requires`, `ensures`, `matches`

**Testing**: `describe`, `assert`

**Note**: The word `describe` is reserved within test block definitions. The word `test` can be used as a function name in any context.

## Error Handling

### Syntax Errors
LX provides detailed error messages for common syntax mistakes:

```lx
# Error: Missing function body
fun my_func(x, y)  # Missing { }

# Error: Reserved word usage in wrong context
fun spec() { }  # 'spec' is reserved for specifications

# Error: Missing description in test
test { }  # Should be: test "description" { }
```

### Variable Assignment Errors
```lx
# Error: Variable reassignment
x = 42
x = 43  # Error: Variable 'x' is already defined in this scope and cannot be reassigned

# Valid: Different scopes
fun test_scoping() {
  x = 42  # This is fine
  result = {
    x = 100  # This is also fine - different scope
    x + 1
  }
  x + result  # Uses outer x (42) and result (101)
}
```

### OTP Validation Errors
The compiler validates OTP callback functions:

```lx
worker bad_worker {
  # Error: init must have exactly 1 parameter
  fun init(x, y) { .{:ok, 0} }

  # Error: handle_call must have exactly 3 parameters
  fun handle_call(request) { .{:reply, :ok, 0} }

  # Error: handle_call must return a tuple
  fun handle_call(req, from, state) { :ok }
}
```

## Compilation Optimizations

### Block Expression Optimization

LX compiles block expressions as inline statements rather than anonymous functions for better performance. This optimization:

- **Eliminates function call overhead** - No anonymous function creation or invocation
- **Improves readability** - Generated Erlang code is more straightforward
- **Maintains proper scoping** - Variables are renamed with unique suffixes to prevent conflicts
- **Preserves semantics** - Block behavior remains identical to the original design

**Example Transformation:**

```lx
fun calculate() {
  result = {
    x = 10
    y = 20
    x + y
  }
  result * 2
}
```

**Generates optimized Erlang:**

```erlang
calculate() ->
    % start block Result_abc
    X_abc = 10,
    Y_abc = 20,
    % end block Result_abc
    Result_def = X_abc + Y_abc,
    Result_def * 2.
```
## Best Practices

1. **Use descriptive names** for functions and variables
2. **Avoid variable reassignment** - use different names or scopes
3. **Use block expressions** for complex calculations
4. **Always provide init function** in workers
5. **Return tuples from OTP callbacks** (except format_status)
6. **Use pattern matching** instead of nested if-else
7. **Write tests** for your functions with descriptive names
8. **Use specifications** to document function contracts
9. **Follow consistent indentation** (2 or 4 spaces)
10. **Use atoms** for status values (`:ok`, `:error`, etc.)
11. **Use `#` for comments**, not `//`
12. **Leverage block expressions** for better code organization and performance

## Examples

### Variable Assignment and Blocks
```lx
fun calculate_total(items) {
  # Calculate subtotal
  subtotal = {
    sum = 0
    # In a real implementation, this would be a fold/reduce
    for item in items {
      sum = sum + item.price
    }
    sum
  }

  # Calculate tax
  tax_rate = 0.08
  tax = subtotal * tax_rate

  # Calculate total
  total = subtotal + tax

  # Return structured result as tuple
  .{subtotal, tax, total}
}
```

### Complex Worker with Assignments
```lx
worker shopping_cart {
  fun init(user_id) {
    initial_state = .{user_id, [], 0.0}
    .{:ok, initial_state}
  }

  fun handle_call(request, from, state) {
    # Simple example - in practice you'd pattern match on request
    response = process_request(request, state)
    .{:reply, response, state}
  }

  fun handle_cast(request, state) {
    # Process async request
    new_state = update_state(request, state)
    .{:noreply, new_state}
  }

  fun handle_info(info, state) {
    .{:noreply, state}
  }

  fun terminate(reason, state) {
    .{:ok}
  }

  fun code_change(old_vsn, state, extra) {
    .{:ok, state}
  }

  # Helper function with arithmetic
  fun calculate_total(items) {
    # Calculate sum using arithmetic operations
    total = 0
    for item in items {
      total = total + item.price * item.quantity
    }
    total
  }
}
```

### Pattern Matching with Assignments
```lx
fun process_api_response(response) {
  parsed_response = case response {
    .{:ok, data} -> {
      # Process successful response
      processed_data = transform_data(data)
      validated_data = validate_data(processed_data)
      .{:success, validated_data}
    }
    .{:error, reason} -> {
      # Handle error response
      error_message = format_error(reason)
      log_error(error_message)
      .{:failure, error_message}
    }
    _ -> {
      # Handle unexpected response
      error_msg = "Unexpected response format"
      log_warning(error_msg)
      .{:failure, error_msg}
    }
  }

  # Return final result
  parsed_response
}
```

### Testing with Multiple Statements
```lx
describe "shopping cart tests" {
  test "should handle basic operations" {
    # Setup
    initial_state = .{123, [], 0.0}

    # Execute
    result = shopping_cart.init(123)

    # Verify
    case result {
      .{:ok, state} -> {
        assert state.0 == 123  # user_id
        assert state.1 == []   # items
        assert state.2 == 0.0  # total
      }
      _ -> assert false  # Should not reach here
    }
  }

  test "should handle arithmetic operations" {
    # Test basic arithmetic
    x = 10
    y = 5

    # Test all operators
    addition = x + y      # 15
    subtraction = x - y   # 5
    multiplication = x * y # 50
    division = x / y      # 2

    # Test precedence
    complex = x + y * 2   # 20 (not 30)
    grouped = (x + y) * 2 # 30

    assert addition == 15
    assert subtraction == 5
    assert multiplication == 50
    assert division == 2
    assert complex == 20
    assert grouped == 30
  }

  test "should support recursive functions" {
    # Test factorial function
    result = factorial(5)
    assert result == 120

    # Test with base case
    base_case = factorial(0)
    assert base_case == 1
  }
}
```

This reference document reflects the current state of the LX language implementation including:

- Variable assignment and block expressions
- Arithmetic operations with proper precedence (`+`, `-`, `*`, `/`)
- Pattern matching in function clauses with literal patterns
- Recursive function support
- Enhanced comment support with `#` syntax
- Improved error handling with contextual messages
- OTP worker and supervisor definitions
- Comprehensive testing framework

The language continues to evolve with new features being added regularly. Recent additions include full arithmetic expression support with proper operator precedence and enhanced pattern matching capabilities for function definitions.