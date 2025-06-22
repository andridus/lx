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
13. [Application Definition](#application-definition)
14. [Build System & Compilation](#build-system--compilation)
15. [Examples](#examples)

## Lexical Elements

### Keywords

LX has several categories of keywords:

#### Core Language Keywords
- `fun` - Function definition
- `pub` - Public function visibility modifier
- `case` - Pattern matching
- `if` - Conditional expression
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
- Start with lowercase letter or underscore
- Can contain letters, digits, and underscores
- Examples: `x`, `my_var`, `count_1`, `_ignored`

#### Special Variable Patterns
- `_` (underscore) - Wildcard pattern, ignores the value
- Variables starting with `_` - Ignored variables for assignments
- `__MODULE__` - Special macro that compiles to `?MODULE` in Erlang

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

#### Ignored Variable Assignments
Variables starting with underscore are treated as ignored assignments - they evaluate the right-hand side for side effects but don't create variable bindings:

```lx
fun example() {
  _result = expensive_computation()  # Evaluates but doesn't bind
  _debug = log_message("debug info") # Side effect only
  :ok
}
```

**Compilation behavior**: Ignored variables are compiled as just the expression value without assignment:
```erlang
% Generated Erlang:
example() ->
    expensive_computation(),
    log_message("debug info"),
    ok.
```

### Variable Scoping Rules

LX has strict scoping rules to prevent common programming errors:

#### 1. Same Scope Redefinition - NOT ALLOWED
```lx
fun example() {
  x = 42
  x = 100  # Error: Variable 'x' is already defined in this scope
}
```

#### 2. Shadowing (Parent-to-Child) - NOT ALLOWED
```lx
fun example() {
  x = 42           # Variable in parent scope
  result = {
    x = 100        # Error: Cannot shadow parent scope variable
    x + 1
  }
}
```

#### 3. Same Name in Sibling Scopes - ALLOWED
```lx
fun example() {
  result1 = {
    x = 100        # OK: Variable in block 1
    x + 10
  }

  result2 = {
    x = 200        # OK: Different scope from block 1
    x + 20
  }

  .{result1, result2}
}
```

**Compilation Behavior**: Each scope gets unique variable names with suffixes to prevent conflicts:

```erlang
% Generated Erlang code:
example() ->
    % start block Result1_abc
    X_abc = 100,
    % end block Result1_abc
    Result1_def = X_abc + 10,
    % start block Result2_ghi
    X_ghi = 200,
    % end block Result2_ghi
    Result2_def = X_ghi + 20,
    {Result1_def, Result2_def}.
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

# Using __MODULE__ macro for self-references
gen_server.call(__MODULE__, :get_state)
```

#### Module Reference Macro
The `__MODULE__` macro is a special identifier that compiles to `?MODULE` in Erlang, providing a reference to the current module:

```lx
fun get_current_module() {
  __MODULE__  # Compiles to ?MODULE in Erlang
}

fun call_self() {
  gen_server.call(__MODULE__, :some_request)
}
```

**Compilation behavior**:
```erlang
% Generated Erlang:
get_current_module() ->
    ?MODULE.

call_self() ->
    gen_server:call(?MODULE, some_request).
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

# Public function (exported in generated Erlang)
pub fun public_add(x, y) {
  x + y
}

# With multiple statements
fun complex_calculation(a, b) {
  temp1 = a * 2
  temp2 = b + 5
  result = temp1 + temp2
  result
}

# With underscore parameters (ignored parameters)
fun init(_args) {
  # Ignore initialization arguments
  .{:ok, initial_state}
}

fun handle_call(:get, _from, state) {
  # Ignore the 'from' parameter
  .{:reply, state, state}
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

### Function Visibility

LX supports function visibility modifiers to control which functions are exported in the generated Erlang code:

#### Private Functions (Default)
```lx
# Private function - not exported
fun internal_helper(x, y) {
  x + y
}

# OTP callbacks are automatically exported regardless of visibility
fun handle_call(request, _from, state) {
  .{:reply, :ok, state}
}
```

#### Public Functions
```lx
# Public function - exported in generated Erlang
pub fun api_function(data) {
  processed = internal_helper(data, 42)
  .{:ok, processed}
}

# Public function in worker
worker my_worker {
  fun init(_) { .{:ok, []} }

  # Public API for external callers
  pub fun get_data(worker_pid) {
    gen_server.call(worker_pid, :get_data)
  }

  pub fun set_data(worker_pid, data) {
    gen_server.call(worker_pid, .{:set_data, data})
  }

  # Private OTP callbacks (automatically exported)
  fun handle_call(:get_data, _from, state) {
    .{:reply, state, state}
  }

  fun handle_call(.{:set_data, data}, _from, _state) {
    .{:reply, :ok, data}
  }
}
```

#### Generated Erlang Exports
```erlang
% Generated from the above example:
-module(my_worker).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, get_data/1, set_data/2]).

% start_link/0 - automatically exported for OTP workers
% init/1, handle_call/3 - OTP callbacks automatically exported
% get_data/1, set_data/2 - public functions explicitly exported
% internal_helper/2 would NOT be exported (private function)
```

#### Export Rules
1. **OTP Callbacks**: Always exported regardless of visibility (`init/1`, `handle_call/3`, etc.)
2. **Public Functions**: Functions marked with `pub` are exported
3. **Private Functions**: Functions without `pub` are not exported (except OTP callbacks)
4. **Worker Start Functions**: `start_link/0` is automatically exported for workers
5. **Supervisor Functions**: `start_link/0` and `init/1` are automatically exported for supervisors

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

  # Optional callback functions with multiple clauses
  fun handle_call(:get_state, _from, state) {
    .{:reply, state, state}
  }

  fun handle_call(.{:set_value, value}, _from, state) {
    new_state = update_state(state, value)
    .{:reply, :ok, new_state}
  }

  fun handle_call(:reset, _from, _state) {
    .{:reply, :ok, initial_state()}
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

  # Public helper functions (exported)
  pub fun get_current_value(worker_pid) {
    gen_server.call(worker_pid, :get_state)
  }

  pub fun set_value(worker_pid, value) {
    gen_server.call(worker_pid, .{:set_value, value})
  }

  # Private helper functions (not exported)
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

Supervisors must use bracket syntax for defining children lists to maintain consistency with list notation throughout the language:

```lx
supervisor my_supervisor {
  strategy one_for_one
  children [worker1, worker2, worker3]
}

# Empty children list
supervisor empty_supervisor {
  strategy one_for_one
  children []
}
```

**Important**: The `children` field must always use bracket notation `[...]` even for empty lists. This ensures consistency with list syntax used elsewhere in LX.

#### Advanced: Typed Children Syntax

LX supports an advanced typed children syntax to resolve ambiguity when workers and supervisors have the same name:

```lx
# Simple syntax (works when no name conflicts)
supervisor simple_sup {
  strategy one_for_one
  children [cart, payment]
}

# Typed syntax (recommended when there are name conflicts)
supervisor advanced_sup {
  strategy one_for_one
  children {
    worker [cart, payment]
    supervisor [cart_manager, payment_manager]
  }
}

# Mixed typed syntax
supervisor mixed_sup {
  strategy one_for_all
  children {
    worker [cart]
    supervisor [payment_manager]
  }
}
```

#### Ambiguity Detection

The LX compiler automatically detects when the same name is used for both a worker and supervisor, preventing ambiguous references:

```lx
worker cart {
  fun init(_) { .{:ok, []} }
}

supervisor cart {
  strategy one_for_one
  children { worker [cart] }
}

# This will cause a compilation error:
supervisor main_supervisor {
  strategy one_for_one
  children [cart]  # ERROR: Ambiguous - cart worker or cart supervisor?
}

# Correct solution using typed syntax:
supervisor main_supervisor {
  strategy one_for_one
  children {
    supervisor [cart]  # Explicitly specify the supervisor
  }
}
```

**Error Message Example:**
```
myapp.lx:15:1: OTP Error: Ambiguous reference 'cart' in supervisor 'main supervisor'
  Problem: 'cart' is used for both worker and supervisor components
  Solution: Use typed children syntax to specify the component type:
    children {
      worker [cart]     # if referring to the worker
      supervisor [cart] # if referring to the supervisor
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

## Application Definition

LX provides a declarative syntax for defining OTP applications. The application definition generates the necessary `.app.src` file, `rebar.config`, and application module automatically.

### Basic Application Definition
```lx
application {
  description "My LX Application"
  vsn "1.0.0"
  applications [kernel, stdlib, crypto]
  registered [my_supervisor]
  env []
}
```

### Application Fields

#### Required Fields
- **description**: String describing the application
- **vsn**: Version string for the application

#### Optional Fields
- **applications**: List of OTP applications this application depends on (defaults to `[kernel, stdlib]`)
- **registered**: List of registered process names (automatically includes supervisors)
- **env**: List of environment variables as key-value pairs

### Complete Application Example
```lx
# File: myapp.lx
application {
  description "My Shopping Cart Application"
  vsn "1.0.0"
  applications [kernel, stdlib, crypto, mnesia]
  registered [cart_supervisor]
  env [max_items: 100, timeout: 5000]
}

# Define worker components
worker cart_worker {
  fun init(user_id) {
    initial_state = .{user_id, [], 0.0}
    .{:ok, initial_state}
  }

  fun handle_call(:get_cart, _from, state) {
    .{:reply, state, state}
  }

  fun handle_cast(.{:add_item, item}, .{user_id, items, total}) {
    new_items = [item | items]
    new_total = total + item.price
    new_state = .{user_id, new_items, new_total}
    .{:noreply, new_state}
  }
}

# Define supervisor components
supervisor cart_supervisor {
  strategy one_for_one
  children [cart_worker]
}
```

### Generated Files

When you compile a LX file with an application definition, the following files are automatically generated:

#### 1. `src/myapp.app.src`
```erlang
{application, myapp, [
  {description, "My Shopping Cart Application"},
  {vsn, "1.0.0"},
  {modules, [myapp_app, cart_supervisor, cart_worker]},
  {registered, [cart_supervisor]},
  {mod, {myapp_app, []}},
  {applications, [kernel, stdlib, crypto, mnesia]},
  {env, []}
]}.
```

#### 2. `rebar.config`
```erlang
{erl_opts, [debug_info]}.
{deps, []}.

{applications, [kernel, stdlib]}.

{project_plugins, []}.
{sub_dirs, []}.
```

#### 3. `src/myapp_app.erl`
```erlang
-module(myapp_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    myapp_sup:start_link().

stop(_State) ->
    ok.
```

### Application Name Inference

The application name is automatically inferred from the filename:
- `myapp.lx` → application name: `myapp`
- `shopping_cart.lx` → application name: `shopping_cart`

### Module Collection

The compiler automatically collects all modules for the `{modules, [...]}` field:
- Application module: `<app_name>_app`
- All workers defined in the file
- All supervisors defined in the file
- All standalone functions (grouped into the main module)

### Registered Processes

The `{registered, [...]}` field is populated by:
1. Automatically detected supervisors
2. User-defined registered processes from the `registered` field
3. Duplicates are automatically removed

### Environment Variables

Environment variables can be defined using the `env` field:
```lx
application {
  description "My App"
  vsn "1.0.0"
  env [
    max_connections: 100,
    timeout: 5000,
    debug_mode: true
  ]
}
```

### Compilation Process

1. **Parse**: LX parses the application definition and OTP components
2. **Generate Erlang**: Converts LX code to Erlang modules
3. **Generate App Files**: Creates `.app.src`, `rebar.config`, and application module
4. **Compile with Rebar**: Use `rebar3 compile` to build the final application

### Automatic Rebar3 Integration

LX now includes automatic rebar3 integration. When you compile an LX file with an application definition, the compiler will:

1. **Generate** the complete OTP application structure
2. **Download** rebar3 automatically if not found (to `~/.lx/rebar3`)
3. **Compile** the project using rebar3
4. **Display** compilation results in LX error format

#### Compilation Process
```bash
# Simple compilation - everything is automatic
lx myapp.lx

# Output example:
# Compiling project...
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling myapp
# Project compiled successfully with rebar3
# Generated application files for myapp
```

#### Rebar3 Management
- **System Detection**: First checks if rebar3 is available in system PATH
- **Auto-Download**: Downloads rebar3 to `~/.lx/rebar3` if not found
- **Version Control**: Uses rebar3 version 3.23.0
- **Cross-Platform**: Works on Linux systems (requires curl or wget)

#### Error Integration
Rebar3 compilation errors are automatically converted to LX's error format:

```
examples/myapp/src/myapp_worker.erl:15:23: Parse Error: Rebar3 compilation error: syntax error before: '}'
Check the generated Erlang code for syntax errors
  Context: This error occurred during rebar3 compilation of generated Erlang code
```

#### Manual Compilation (Optional)
The generated files remain fully compatible with manual rebar3 usage:

```bash
# Navigate to generated project
cd myapp

# Manual compilation
rebar3 compile

# Run tests
rebar3 eunit

# Create a release
rebar3 release

# Start the application
rebar3 shell
```
```

## Examples

### Complete Application with Automatic Compilation

```lx
# File: shopping_app.lx
application {
  description "E-commerce Shopping Application"
  vsn "2.1.0"
  applications [kernel, stdlib, crypto, mnesia]
}

worker cart_manager {
  fun init(user_id) {
    initial_state = .{user_id, [], 0.0}
    .{:ok, initial_state}
  }

  fun handle_call(:get_cart, _from, state) {
    .{:reply, state, state}
  }

  fun handle_cast(.{:add_item, item}, .{user_id, items, total}) {
    new_items = [item | items]
    new_total = total + item.price
    new_state = .{user_id, new_items, new_total}
    .{:noreply, new_state}
  }

  fun handle_info(_info, state) {
    .{:noreply, state}
  }

  fun terminate(_reason, _state) {
    .{:ok}
  }
}

supervisor cart_supervisor {
  strategy one_for_one
  children [cart_manager]
}
```

```bash
# Compile the complete application with one command
lx shopping_app.lx

# Output:
# Compiling project...
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling shopping_app
# Project compiled successfully with rebar3
# Generated application files for shopping_app

# Generated project structure:
# shopping_app/
# ├── rebar.config
# ├── src/
# │   ├── shopping_app.app.src
# │   ├── shopping_app_app.erl
# │   ├── shopping_app_supervisor.erl
# │   └── shopping_app_cart_manager_worker.erl
# ├── test/
# │   └── shopping_app_SUITE.erl
# └── _build/
#     └── default/
#         └── lib/
#             └── shopping_app/
#                 └── ebin/
#                     ├── shopping_app.app
#                     └── *.beam files
```

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

### Complex Worker with Special Syntax Features
```lx
worker shopping_cart {
  fun init(_args) {
    # Underscore parameter ignores initialization arguments
    initial_state = .{[], 0.0}
    .{:ok, initial_state}
  }

  fun handle_call(request, _from, state) {
    # Underscore parameter ignores the 'from' reference
    case request {
      :get_cart -> .{:reply, state, state}
      :get_module -> .{:reply, __MODULE__, state}  # Use __MODULE__ macro
      _ -> .{:reply, :unknown_request, state}
    }
  }

  fun handle_cast(request, state) {
    # Process async request with ignored variables
    _log_result = log_request(request)  # Side effect only
    new_state = update_state(request, state)
    .{:noreply, new_state}
  }

  fun handle_info(_info, state) {
    # Ignore info messages
    .{:noreply, state}
  }

  fun terminate(_reason, state) {
    # Ignore termination reason, just cleanup
    _cleanup_result = cleanup_resources(state)
    .{:ok}
  }

  fun code_change(_old_vsn, state, _extra) {
    # Ignore version and extra data
    .{:ok, state}
  }

  # Helper function demonstrating external calls
  fun notify_external_service(data) {
    # Use dot notation for external module calls
    http_client.post("http://api.example.com", data)
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

## Recent Improvements

### Public Function Support & Export System Overhaul (Latest)

LX now includes comprehensive support for function visibility with automatic export generation, eliminating the use of `export_all` and providing precise control over module interfaces.

#### Key Features
- **`pub` keyword**: Mark functions as public for export in generated Erlang
- **Automatic export generation**: No more `export_all` - only necessary functions are exported
- **OTP callback detection**: Automatically exports required OTP callbacks
- **Multiple function clauses**: Properly handles multiple clauses of the same function name
- **Precise export lists**: Generates clean `-export([...])` directives

#### Function Visibility System
```lx
worker calculator {
  # Public API functions - exported
  pub fun add(a, b) { a + b }
  pub fun multiply(a, b) { a * b }

  # Private helper - not exported
  fun validate_input(x) { x > 0 }

  # OTP callbacks - automatically exported
  fun init(_) { .{:ok, 0} }

  fun handle_call(:get, _from, state) {
    .{:reply, state, state}
  }

  fun handle_call(.{:add, value}, _from, state) {
    .{:reply, state + value, state + value}
  }
}
```

#### Generated Erlang (Clean Exports)
```erlang
-module(calculator_worker).
-behaviour(gen_server).
-export([add/2, multiply/2, start_link/0, init/1, handle_call/3]).

% Multiple handle_call clauses properly generated
handle_call(get, _From, State) ->
    {reply, State, State};

handle_call({add, Value}, _From, State) ->
    {reply, State + Value, State + Value}.

% Public functions exported
add(A, B) -> A + B.
multiply(A, B) -> A * B.

% validate_input/1 NOT exported (private function)
```

#### Benefits
- **Security**: Only intended functions are accessible externally
- **Clean interfaces**: Clear separation between public API and internal implementation
- **OTP compliance**: Follows Erlang/OTP best practices for module exports
- **Maintainability**: Easy to understand what functions are part of the public API

### Advanced Ambiguity Detection & Typed Children Syntax

The LX compiler now includes comprehensive ambiguity detection for OTP components, ensuring developers receive clear, actionable feedback when name conflicts occur:

#### Enhanced OTP Validation
- **Ambiguity detection**: Automatically detects when the same name is used for both worker and supervisor
- **Precise error reporting**: Shows exact file location with line:column format
- **Clear error messages**: Uses "main supervisor" instead of technical terms
- **Practical solutions**: Provides specific syntax examples for resolution

#### Typed Children Syntax
- **Simple syntax**: `children [worker1, worker2]` for basic cases
- **Typed syntax**: `children { worker [...], supervisor [...] }` for complex scenarios
- **Mixed support**: Can combine worker and supervisor children in one declaration
- **Backward compatibility**: Existing simple syntax continues to work when no conflicts exist

#### Error Message Improvements
```lx
# Before: Compilation succeeds with potential runtime issues
# After: Clear compile-time error with solution

supervisor {
  children [cart]  # Error if both worker and supervisor named 'cart' exist
}

# Error output:
# myapp.lx:10:1: OTP Error: Ambiguous reference 'cart' in supervisor 'main supervisor'
#   Problem: 'cart' is used for both worker and supervisor components
#   Solution: Use typed children syntax to specify the component type:
#     children {
#       worker [cart]     # if referring to the worker
#       supervisor [cart] # if referring to the supervisor
#     }
```

#### Technical Implementation
- **AST enhancement**: Added position tracking to OTP components
- **Parser updates**: Captures exact line/column information for error reporting
- **Validation engine**: Intelligent type-aware dependency checking
- **Test coverage**: 116+ comprehensive tests including ambiguity scenarios

### Enhanced Supervisor Error Testing (Previous)

The LX compiler includes comprehensive test coverage for supervisor error messages, ensuring developers receive clear, actionable feedback when syntax errors occur:

#### Test Suite Expansion
- **7 new supervisor-specific tests** added to the test suite
- **Total test count increased to 115+** comprehensive tests
- **Error message validation** ensures consistency and helpfulness
- **Position tracking verification** confirms accurate line/column reporting

#### Error Message Testing Categories

1. **Syntax Error Detection**: Tests verify that missing brackets are properly detected
2. **Suggestion Validation**: Tests ensure helpful suggestions are provided in error messages
3. **Context Information**: Tests verify educational context about why brackets are required
4. **Position Accuracy**: Tests confirm errors are reported at correct line and column numbers
5. **Positive Validation**: Tests ensure correct syntax parses successfully
6. **Edge Cases**: Tests validate empty children lists and various syntax variations

#### Example Test Coverage

```lx
# Test case: Missing brackets with multiple children
supervisor cart_sup {
  strategy one_for_one
  children cart, inventory, payment  # Error: brackets required
}
# Expected: Clear error message with suggestion to use [cart, inventory, payment]

# Test case: Correct syntax validation
supervisor cart_sup {
  strategy one_for_one
  children [cart, inventory, payment]  # Success: parses correctly
}
# Expected: Successful parsing with proper AST generation
```

#### Error Message Structure Validation

The tests verify that error messages include:
- **Primary message**: Clear description of the syntax error
- **Suggestion field**: Specific guidance on how to fix the error
- **Context field**: Educational information about why the syntax is required
- **Position information**: Accurate line and column numbers

This comprehensive testing ensures that developers receive consistent, helpful error messages that accelerate the learning process and reduce debugging time.

## Build System & Compilation

### Enhanced Build Directory Management

LX now provides intelligent build directory management with automatic cleanup of old artifacts:

#### Unified Build Structure
- **Application files**: Creates `_build/project_name/` with OTP structure (`src/`, `test/`, `rebar.config`)
- **Non-application files**: Creates `_build/filename/` with generated `.erl` files directly
- **Automatic cleanup**: Removes old build artifacts before each compilation

#### Build Directory Examples

```bash
# Application file compilation
lx myapp.lx
# Creates: _build/myapp/ with OTP structure

# Non-application file compilation
lx calculator.lx
# Creates: _build/calculator/ with Erlang files

# Automatic cleanup when switching types
lx myapp.lx        # Creates application structure
# Edit file to remove application definition
lx myapp.lx        # Automatically cleans up old OTP structure
```

### Compilation Flags

#### `--skip-rebar` Flag

The `--skip-rebar` flag allows you to skip the rebar3 compilation step for faster development and testing:

```bash
# Standard compilation (generates files + runs rebar3)
lx myapp.lx

# Skip rebar3 compilation (generates files only)
lx --skip-rebar myapp.lx

# Combine with type checking
lx --type-check --skip-rebar myapp.lx
```

**Use Cases for `--skip-rebar`:**
- **Fast development**: Quick syntax checking without full compilation
- **Testing**: Faster test execution (100x speed improvement)
- **CI/CD**: Separate generation and compilation steps
- **Debugging**: Inspect generated Erlang code without compilation

#### `--type-check` Flag

Performs type checking without code generation:

```bash
# Type check only
lx --type-check myapp.lx

# Type check with skip rebar (fastest validation)
lx --type-check --skip-rebar myapp.lx
```

### Compilation Workflow

#### Standard Workflow
1. **Parse** LX source code
2. **Type check** (if enabled)
3. **Generate** Erlang code and build structure
4. **Cleanup** old build artifacts
5. **Compile** with rebar3 (unless skipped)

#### Development Workflow with `--skip-rebar`
1. **Parse** LX source code
2. **Type check** (if enabled)
3. **Generate** Erlang code and build structure
4. **Cleanup** old build artifacts
5. **Skip** rebar3 compilation

#### Manual Compilation After Generation
```bash
# Generate files without compilation
lx --skip-rebar myapp.lx

# Navigate to generated project
cd _build/myapp

# Manual compilation
rebar3 compile

# Run tests
rebar3 eunit

# Start shell
rebar3 shell
```

### Build Artifact Management

#### Automatic Cleanup Process
- **Detection**: Identifies old build artifacts before compilation
- **Removal**: Uses `rm -rf` with OCaml fallback for reliability
- **Recreation**: Creates fresh build structure for current compilation
- **Safety**: Only removes artifacts in `_build/` directory

#### Cleanup Scenarios
- **Application to non-application**: Removes OTP structure when application definition is removed
- **Non-application to application**: Removes simple structure when application definition is added
- **Same type recompilation**: Cleans up old artifacts for fresh compilation
- **Missing directories**: Handles missing build directories gracefully

### Performance Improvements

#### Test Suite Performance
- **Before**: 2.5 seconds with rebar3 compilation
- **After**: 0.026 seconds with `--skip-rebar` (100x faster)
- **Benefit**: Dramatically faster development cycles

#### Development Benefits
- **Faster feedback**: Quick syntax validation
- **Cleaner workspace**: Automatic artifact cleanup
- **Flexible workflow**: Choose when to run full compilation
- **Better debugging**: Inspect generated code without compilation overhead

### Error Handling

#### Build System Errors
```bash
# Example error with cleanup context
lx myapp.lx
# Output: myapp.lx:15:23: Build Error: Failed to cleanup old artifacts
#         Context: Error occurred during build directory management
#         Solution: Check file permissions in _build/ directory
```

#### Compilation Errors with `--skip-rebar`
```bash
# Skip rebar3 but still validate syntax
lx --skip-rebar myapp.lx
# Output: myapp.lx:10:15: Syntax Error: Missing closing brace
#         Context: Error in function definition
#         Note: Rebar3 compilation was skipped
```

### Integration with Existing Tools

#### Makefile Integration
```makefile
# Fast development target
dev:
	lx --skip-rebar src/myapp.lx

# Full compilation target
build:
	lx src/myapp.lx

# Type check only
check:
	lx --type-check src/myapp.lx
```

#### CI/CD Integration
```yaml
# Example GitHub Actions workflow
- name: Fast validation
  run: lx --type-check --skip-rebar src/myapp.lx

- name: Generate artifacts
  run: lx --skip-rebar src/myapp.lx

- name: Compile with rebar3
  run: cd _build/myapp && rebar3 compile
```

This enhanced build system provides developers with flexible, efficient compilation options while maintaining the robustness and reliability of the LX toolchain.