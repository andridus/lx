## Lx Language Syntax Reference

Lx is a statically scoped, expression-based functional language designed for building robust OTP applications that run on the Erlang/BEAM VM. It embraces immutable data, lightweight processes, and Erlang interoperability, with an ergonomic and expressive syntax.

---

### Table of Contents

1. Keywords — reserved words grouped by purpose
2. Directives — compile-time metadata and control
3. Operators and Punctuation — syntax markers and behavior
4. Literals — constants like numbers, strings, atoms
5. Identifiers — naming conventions for variables and modules
6. Comments — inline documentation syntax
7. Expressions — fundamental program building blocks
7.1. Type Aliases and Type Annotations — type aliases, variable and parameter annotations
8. Pattern Matching — control flow via structural decomposition
9. Function Definitions — single and multi-clause declarations
9.1. Fun Expressions — anonymous functions with closures and higher-order support
10. Guards — conditional clauses in pattern matching
11. Message Passing — concurrent communication between processes
12. Receive Expressions — selective message handling
13. Control Flow — conditional constructs and loops
14. Data Structures — working with tuples and lists
15. Records — structured data with named fields
16. OTP Components — defining supervisors and workers
17. Specifications — declaring contracts for validation
18. Testing — structure and assertions for test blocks
19. Application Definition — project-level metadata and setup
20. Build System — compiling and generating artifacts
21. Module System and Dependencies — declaring dependencies and integrating with external modules
22. Error Handling and Suggestions — comprehensive error reporting with didactic examples

---

### 1. Keywords

These reserved words define control flow, type contracts, concurrency, and OTP structure. They cannot be redefined.

- **Core**: `def`, `defp`, `case`, `if`, `else`, `do`, `end`, `with`, `for`, `when`, `receive`, `after`, `true`, `false`, `nil`, `unsafe`
- **Block Delimiters**: `do`, `end` — define code blocks for functions, control flow, and scoped expressions
- **Data**: `record` — structured data type definitions
- **OTP**: `worker`, `supervisor`, `strategy`, `children`, `one_for_one`, `one_for_all`, `rest_for_one`
- **Specification**: `spec`, `requires`, `ensures`, `matches`
- **Testing**: `describe`, `test`, `assert`

---

### 2. Directives

Directives provide compile-time metadata and control compilation behavior. They are prefixed with `@` and must be placed immediately before function definitions.

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

#### Reflection Output and Type Aliases

The `@reflection` directive now preserves user-defined type aliases in its output. When a function parameter or return type uses a type alias (e.g., `int`), the reflection output will show the alias name in the function signature and for parameter variables in the body, instead of the resolved base type (e.g., `integer`).

**Example:**

```lx
# Define a type alias
type int :: integer

@reflection
def convert(x :: int) do
  x
end
```

**Reflection output:**

```
=== REFLECTION INFO function: convert ===
  convert(x :: int) :: int [guard: nil]
    Var(x) :: int
=== END REFLECTION INFO ===
```

This makes the type information clearer and more faithful to the source code, especially when using domain-specific type aliases.

#### Guard Expression Formatting

The reflection output also provides clean, readable formatting for guard expressions. Instead of showing verbose internal AST representations, guards are displayed as simple, readable expressions.

**Examples:**

```lx
@reflection
def with_guard(x :: int) when x > 0 do
  x
end

@reflection
def without_guard(x :: int) do
  x
end
```

**Reflection output:**

```
=== REFLECTION INFO function: with_guard ===
  with_guard(x :: int) :: int [ x > 0]
    Var(x) :: int
=== END REFLECTION INFO ===
=== REFLECTION INFO function: without_guard ===
  without_guard(x :: int) :: int
    Var(x) :: int
=== END REFLECTION INFO ===
```

Notice that:
- Functions with guards show the guard expression in brackets: `[ x > 0]`
- Functions without guards show no guard section
- Guard expressions are displayed in a clean, readable format

#### Directive Syntax Rules

- Directives must start with `@` followed by the directive name
- Directives must be placed immediately before a function definition
- Multiple directives can be used on the same function
- Directives are processed during the type checking phase
- Unknown directives are ignored (with potential future warnings)

#### Examples

```lx
# Single directive
@reflection
def process_data(input) do
  result = transform(input)
  validate(result)
end

# Multiple directives
@reflection
@inline
def fast_calculation(x, y) do
  x * y + x + y
end

# Directives with complex functions
@reflection
def complex_function(data) when is_list(data) do
  case data do
    [head | tail] -> process_list(head, tail)
    [] -> :empty
    _ -> :unknown
  end
end
```

---

### 3. Operators and Punctuation

Syntax symbols used for operations, declarations, and structure:

- **Assignment**: `=` — bind a value to a variable once
- **Pattern matching**: `<-` — explicit pattern matching operator (recommended for maps)
- **With expression binding**: `<=` — bind patterns in with expressions for monadic-style error handling
- **Unsafe pattern matching**: `unsafe` — bypass all type checking and validation
- **Pattern branching**: `->` — used in `case` and `receive`
- **Message send**: `!` — send messages between processes
- **Type annotation / list cons**: `::` — optional type hints and list construction
- **Module access**: `.` — call module functions
- **String concatenation**: `++`
- **Record update**: `|` — update record fields (used in `{record | field: value}`)
- **Map creation**: `%{}` — create maps with `key: value` (atoms) or `key => value` (general)
- **Map access**: `[]` — access map values with `map[key]` or `map[:atom_key]`
- **Math**: `+`, `-`, `*`, `/`
- **Unary**: `-` (negation), `not` (logical negation)
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logic**: `and`, `or`, `not`, `andalso`, `orelse`
- **Grouping and data**: `()`, `{}`, `[]`
- **Block delimiters**: `do`, `end` — define code blocks for functions and control flow
- **Separators**: `,`, `;`

#### Operator Precedence

Lx follows a clear operator precedence hierarchy to ensure predictable expression evaluation. Operators are listed from lowest to highest precedence:

| Precedence | Operator | Description | Example |
|------------|----------|-------------|---------|
| 1 (lowest) | `=` | Assignment | `x = y + z` |
| 2 | `!` | Message send | `pid ! message` |
| 3 | `or`, `orelse`, `\|\|` | Logical OR | `a or b` |
| 4 | `and`, `andalso`, `&&` | Logical AND | `a and b` |
| 5 | `==`, `!=`, `<`, `>`, `<=`, `>=` | Comparison | `x == y` |
| 6 | `+`, `-`, `++` | Addition, subtraction, concatenation | `a + b` |
| 7 | `*`, `/` | Multiplication, division | `a * b` |
| 8 | `-`, `not` | Unary minus, logical negation | `-x`, `not flag` |
| 9 (highest) | Function calls, field access | `f()`, `record.field` | `user.name` |

**Key Rules:**
- **Lower precedence operators** bind less tightly than higher precedence ones
- **Parentheses** `()` can be used to override precedence
- **Message send** has very low precedence, requiring parentheses for complex expressions
- **Logical operators** follow standard boolean algebra precedence (`and` before `or`)

**Examples:**
```lx
# Without parentheses - follows precedence rules
pid ! a or b        # Equivalent to: (pid ! a) or b
x = a + b * c       # Equivalent to: x = a + (b * c)
a and b or c        # Equivalent to: (a and b) or c
result = a + -b     # Equivalent to: result = a + (-b)
flag = not x > 0    # Equivalent to: flag = (not x) > 0

# With explicit parentheses for clarity
pid ! (a or b)      # Send the result of (a or b) to pid
x = (a + b) * c     # Multiply the sum by c
a and (b or c)      # AND a with the result of (b or c)
result = a + (-b)   # Explicit unary minus
flag = not (x > 0)  # Logical negation of comparison
```

**Generated Erlang Code:**
The compiler automatically adds parentheses in the generated Erlang code when needed to preserve the correct precedence, ensuring that complex expressions evaluate correctly:

```lx
# Lx source
pid ! a or b
```

```erlang
% Generated Erlang with automatic parentheses
Pid ! (A or B)
```

This ensures that the Erlang code maintains the same semantic meaning as the original Lx code, even when operator precedence differs between the two languages.

---

### 4. Literals

Immutable constant values available in source code:

- **Strings**: `"hello"`, supports escapes (`\n`, `\t`, `\r`, `\"`, `\\`)
- **Integers**: `42`, `-3`, `0`, `1024`
- **Floats**: `3.14`, `-1.0`, `0.0`, `-42.5`
- **Booleans**: `true`, `false`
- **Atoms (symbols)**: `:ok`, `:error`, `:timeout`
- **Nil/null value**: `nil`

#### Numeric Literals with Unary Operators

Numbers can be used with unary operators for negation and complex expressions:

```lx
# Negative literals
negative_int = -42
negative_float = -3.14159
zero = -0

# Unary operators in expressions
calculation = x * -1
result = -value + offset
complex = -(a + b) * c

# Boolean negation
is_false = not true
is_invalid = not (x > 0 and y < 10)
```

#### String Literals

Strings are enclosed in double quotes and support escape sequences:

```lx
# Basic strings
message = "Hello, world!"

# Strings with email addresses and special characters
email = "user@example.com"
path = "C:\\Users\\Name"

# Escape sequences
newline = "Line 1\nLine 2"
tab = "Column 1\tColumn 2"
quote = "He said \"Hello\""
backslash = "Path: C:\\Users\\Name"

# Complex strings with various characters
complex = "Hello @world! This is a test string with symbols."
```

**Supported escape sequences:**
- `\n` - newline
- `\t` - tab
- `\r` - carriage return
- `\"` - double quote
- `\\` - backslash

**Valid characters:** Strings support all printable ASCII characters, including special symbols like `@` for email addresses and other common use cases.

---

### 5. Identifiers

Naming conventions for program symbols:

- Variables: must start lowercase or with `_`, e.g., `count`, `_unused`
- Modules: lowercase, used in `:module.function()`
- Records: must start uppercase, e.g., `Person`, `UserData`
- Special: `__MODULE__` expands to the current module name

---

### 6. Comments

Inline documentation using `#`:

```lx
# This is a comment
value = 10  # Inline comment
```

No multiline comment syntax is supported.

---

### 7. Expressions

All code is built from expressions. Every block or function evaluates to a value.

#### Assignment (immutable):

```lx
x = 10
x = 20  # Error: reassignment not allowed
```

#### Assignment in function bodies

You can assign values to variables inside function bodies:

```lx
def a() do
  name = "Alice"
  age = 30
  is_active = true
end
```

Multiple assignments are allowed in sequence. Each variable can only be assigned once per scope.

#### Variable Scope Rules

Lx enforces strict variable scope rules to ensure code clarity and prevent common errors:

**Variable Declaration:**
- Variables must be declared before use
- Each variable can only be assigned once per scope
- Variables are immutable after assignment

**Scope Validation:**
- The compiler automatically checks variable scope during compilation
- Undefined variables are detected with precise error messages showing line and column
- Reassignment attempts are caught and reported with helpful suggestions

**Error Examples:**
```lx
# Error: Variable 'x' is not defined
def test() do
  y = x + 1  # x is not defined
end

# Error: Variable 'count' cannot be reassigned
def counter() do
  count = 1
  count = 2  # Reassignment not allowed
end

# Error: Variable 'name' shadows variable from outer scope
def process() do
  name = "global"
  if true do
    name = "local"  # Shadowing not allowed
  end
end
```

**Correct Usage:**
```lx
def correct_example() do
  # Declare variables before use
  x = 10
  y = x + 1  # x is defined

  # Use different names for different values
  count1 = 1
  count2 = 2

  # Use block scoping for temporary variables
  result = do
    temp = x * 2
    temp + y
  end
end
```

The compiler provides detailed error messages with suggestions for fixing variable scope issues, making it easier to write correct and maintainable code.

#### Block expressions:

Code blocks are now defined using `do` and `end` keywords for creating scoped computation blocks:

```lx
# Basic block expression
result = do
  a = 1
  b = 2
  a + b  # Last expression is returned
end

# Multiple block expressions
def calculate() do
  result1 = do
    x = 10
    y = 20
    x * y
  end
  result2 = do
    a = 5
    b = 3
    a + b
  end
  result1 + result2
end

# Nested block expressions
def complex_calculation() do
  outer_result = do
    inner_value = do
      base = 10
      multiplier = 2
      base * multiplier
    end
    adjustment = 5
    inner_value + adjustment
  end
  outer_result
end
```

**Block Expression Features:**
- **Scoped Variables**: Variables defined inside blocks are scoped to that block and don't leak to outer scope
- **Return Value**: The last expression in a block is automatically returned as the block's value
- **Variable Shadowing**: Inner blocks can shadow variables from outer scopes without affecting the outer variables
- **Assignment Integration**: Block expressions can be assigned to variables using `variable = do ... end` syntax
- **Nested Support**: Blocks can be nested to any depth with proper scope isolation

**Generated Erlang Code:**

Block assignments are unfolded inline in the generated Erlang code with clear comments marking block boundaries:

```lx
# LX source
def calculate() do
  result = do
    x = 10
    y = 20
    x * y
  end
  result + 5
end
```

```erlang
% Generated Erlang
calculate() ->
% Block of Result
X = 10,
Y = 20,
Result = X * Y
% End Block of Result,
Result + 5.
```

**Multiple Block Example:**

```lx
# LX source with multiple blocks
def multi_block() do
  first = do
    a = 1
    b = 2
    a + b
  end
  second = do
    c = 3
    d = 4
    c * d
  end
  first + second
end
```

```erlang
% Generated Erlang with inline blocks
multi_block() ->
% Block of First
A = 1,
B = 2,
First = A + B
% End Block of First,
% Block of Second
C = 3,
D = 4,
Second = C * D
% End Block of Second,
First + Second.
```

**Benefits:**
- **Clean Output**: Generated Erlang code is clean and readable, avoiding unnecessary `begin...end` blocks
- **Inline Generation**: Block assignments are unfolded inline with clear comment markers
- **Type Safety**: Block expressions participate in type inference, with the type determined by the last expression
- **Scope Isolation**: Variables in blocks are properly scoped and don't interfere with outer scope variables

#### Function calls:

Lx supports both internal and external function calls with clear syntax distinction:

**Internal function calls:**
```lx
sum(3, 4)
factorial(5)
process_data(input)
```

**External function calls (module functions):**
```lx
:io.format("Hello, ~s", ["World"])
:lists.map(fn(x) do x * 2 end, [1, 2, 3])
:erlang.process_info(self())
:crypto.strong_rand_bytes(16)
```

**Key differences:**
- **Internal calls**: `function(args...)` - calls functions defined in the current module
- **External calls**: `:module.function(args...)` - calls functions from other modules
- **Record access**: `variable.field` - accesses record fields (not function calls)

The external call syntax provides clear disambiguation between:
- `person.name` - record field access
- `:io.format("msg")` - external function call
- `format("msg")` - internal function call

---

### 7.1. Type Aliases and Type Annotations

Lx supports type aliases and explicit type annotations for variables and function parameters, providing more control and documentation for your code's types.

#### Type Aliases

Type aliases allow you to define a new name for a type or a union of types. This is useful for readability and reuse.

**Syntax:**
```lx
type name :: type_expression
type opaque name :: type_expression
type nominal name :: type_expression
```

**Examples:**
```lx
type int :: integer
type number :: float | integer
type id :: string | integer
type pair :: {integer, integer}
type user_map :: %{name: string, age: integer}

# Opaque type aliases (implementation details hidden)
type opaque user_id :: integer
type opaque session_token :: string

# Nominal type aliases (distinct types even with same structure)
type nominal celsius :: float
type nominal fahrenheit :: float
```

**Type Alias Modifiers:**

- **`type`**: Regular type alias - the type is transparent and can be used interchangeably with its definition
- **`opaque`**: Opaque type alias - the implementation details are hidden, providing better encapsulation and preventing direct access to the underlying type
- **`nominal`**: Nominal type alias - creates a distinct type even if it has the same structure as another type, preventing accidental mixing of semantically different types

**Benefits of Type Modifiers:**

```lx
# Regular type - transparent
type user_id :: integer
def get_user(id :: user_id) do ... end

# Opaque type - implementation hidden
type opaque session_token :: string
def create_session(user :: user_id) do
  # Internal implementation can change without affecting external code
  generate_token()
end

# Nominal types - prevents mixing semantically different types
type nominal celsius :: float
type nominal fahrenheit :: float

def convert_c_to_f(temp :: celsius) do
  # This prevents accidentally passing fahrenheit to a celsius function
  temp * 9 / 5 + 32
end
```

**Generated Erlang Code:**
```erlang
-type int() :: integer().
-opaque user_id() :: integer().
-nominal celsius() :: float().
```

**Erlang Compatibility:**
- All type aliases generate standard Erlang type declarations
- `opaque` and `nominal` types are fully compatible with Erlang's type system
- Generated specs use standard Erlang spec syntax and are compatible with Dialyzer
- Type aliases can be used in external function calls and module dependencies

You can use type aliases anywhere a type is expected, including annotations and records.

#### Type Annotations in Assignments

You can annotate the type of a variable at assignment time:

**Syntax:**
```lx
x :: int = 1
name :: string = "Alice"
user :: user_map = %{name: "Alice", age: 30}
```

If the value does not match the annotated type, a type error will be reported at compile time.

#### Type Annotations in Function Parameters

Function parameters can be annotated with types or type aliases:

**Syntax:**
```lx
def add(a :: int, b :: number) do
  a + b
end

def greet(name :: string) do
  "Hello, " ++ name
end
```

You can also use type variables for generic functions:
```lx
def identity(x :: a) do
  x
end
```

#### Type Annotations in Records

Record fields can be annotated with types or type aliases:
```lx
record Person {
  name :: string,
  age :: integer
}
```

#### Summary Table
| Context         | Syntax Example                        |
|----------------|---------------------------------------|
| Type alias     | `type number :: float | integer`       |
| Assignment     | `x :: int = 1`                        |
| Function param | `def f(a :: int) do ... end`           |
| Record field   | `record R { f :: int }`               |

#### Notes
- Type annotations are optional; if omitted, types are inferred.
- Type aliases can be recursive and can use unions, tuples, lists, maps, etc.
- The `::` operator is used for both type annotation and list cons. The parser distinguishes by context.
- Type aliases with modifiers (`opaque`, `nominal`) integrate seamlessly with the existing type system and are used in automatic spec generation.
- All type aliases are validated at compile time and generate appropriate Erlang type declarations.

#### Error Example
```lx
x :: string = 123  # Error: 123 is not a string
```

#### Didactic Example
```lx
type id :: integer | string
def show_id(x :: id) do
  "ID: " ++ x
end
```

---

### 8. Pattern Matching

Used for destructuring and conditional logic based on shape:

```lx
case msg do
  :ok -> handle_ok()
  [head | tail] -> handle_list(head, tail)
  {x, y} -> sum(x, y)
  %{ name: user_name, age: user_age } -> process_user(user_name, user_age)
  _ -> :default
end
```

#### Pattern Matching with Maps:

```lx
# Direct assignment with pattern matching
%{ name: user_name, age: user_age } = user_data

# Pattern matching operator (recommended for clarity)
%{ name: user_name, age: _user_age } <- user_data

# Partial pattern matching (extract only needed fields)
%{ status: status } <- response

# Mixed key types in patterns
%{ :type => entity_type, "id" => entity_id } <- entity

# Nested map pattern matching
%{ user: %{ profile: %{ settings: %{ theme: theme } } } } <- app_state
```

---

### 9. Function Definitions

Functions now use `do`/`end` syntax instead of curly braces. Lx supports both public and private functions:

#### Public Functions (`def`):

Public functions are exported and can be called from other modules:

```lx
def greet(name) do "Hello, " ++ name end
def factorial do
  (0) do 1 end
  (N) when N > 0 do N * factorial(N - 1) end
end
```

**Multi-Clause Function Validation:**
- Multi-clause functions (`def func do ... end`) must contain at least one clause with parameters
- Each clause must follow the pattern `(params) -> body` or `(params) when guard -> body`
- Invalid: `def func do a end` (no clauses)
- Valid: `def func do (x) -> x + 1 end` (single clause)
- Valid: `def func do (x) -> x + 1; (y) -> y * 2 end` (multiple clauses)

#### Private Functions (`defp`):

Private functions are internal to the module and are not exported. They cannot be called from other modules:

```lx
defp ping() do :pong end
defp internal_helper(data) do
  # Internal implementation details
  process_data(data)
end
```

**Multi-Clause Function Validation:**
- Same validation rules apply to private functions (`defp`)
- Multi-clause private functions must contain at least one clause with parameters
- Invalid: `defp func do a end` (no clauses)
- Valid: `defp func do (x) -> x + 1 end` (single clause)

**Key differences:**
- **Public functions** (`def`): Exported in the generated Erlang module, can be called from other modules
- **Private functions** (`defp`): Not exported, only accessible within the same module
- Both support the same syntax for single and multiple clauses
- Both support guards and pattern matching
- Both require proper validation: multi-clause functions must have at least one clause with parameters
- Private functions are useful for internal helper functions and implementation details

---

### 9.1. Fun Expressions (Anonymous Functions)

Fun expressions create anonymous functions using `do`/`end` syntax:

#### Simple Fun Expressions:

```lx
# Basic anonymous function
add = fn(x, y) do x + y end
result = add(3, 4)  # Returns 7

# Fun expression as argument to higher-order function
numbers = [1, 2, 3, 4, 5]
doubled = map(numbers, fn(x) do x * 2 end)

# Fun expression with closure (captures variables from enclosing scope)
multiplier = 3
scale = fn(x) do x * multiplier end
scaled_value = scale(10)  # Returns 30
```

#### Multi-Clause Fun Expressions:

Fun expressions can have multiple clauses with pattern matching and optional guards:

```lx
# Pattern matching fun expression
process = fn do
  (:ok) do "Success" end
  (:error) do "Failed" end
  (_) do "Unknown" end
end

# Fun expression with guards
validate = fn do
  (x) when x > 0 do :positive end
  (x) when x < 0 do :negative end
  (0) do :zero end
end

# Complex pattern matching with destructuring
handle_message = fn do
  ({:user, name, age}) when age >= 18 do "Adult: " ++ name end
  ({:user, name, age}) when age < 18 do "Minor: " ++ name end
  ({:system, msg}) do "System: " ++ msg end
  (_) do "Unknown message" end
end
```

#### Higher-Order Functions:

Fun expressions enable powerful functional programming patterns:

```lx
# Function that returns a function
make_adder = fn(n) do
  fn(x) do x + n end
end

add5 = make_adder(5)
result = add5(10)  # Returns 15

# Function that takes a function as argument
apply_twice = fn(f, x) do
  f(f(x))
end

double = fn(x) do x * 2 end
result = apply_twice(double, 3)  # Returns 12

# Functional composition
compose = fn(f, g) do
  fn(x) do f(g(x)) end
end

add_one = fn(x) do x + 1 end
multiply_two = fn(x) do x * 2 end
add_then_multiply = compose(multiply_two, add_one)
result = add_then_multiply(5)  # Returns 12 ((5 + 1) * 2)
```

#### Practical Applications:

```lx
# Event handler with state
def create_counter() do
  count = 0
  fn() do
    count = count + 1
    count
  end
end

# List processing with fun expressions
def filter_map(list, predicate, mapper) do
  list
  |> filter(predicate)
  |> map(mapper)
end

# Usage example
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
even_doubled = filter_map(
  numbers,
  fn(x) do x % 2 == 0 end,  # Filter even numbers
  fn(x) do x * 2 end        # Double them
)
# Result: [4, 8, 12, 16, 20]

# Callback-style programming
def async_operation(data, callback) do
  # Simulate async work
  result = process_data(data)
  callback(result)
end

# Usage with fun expression
async_operation(my_data, fn(result) do
  case result do
    :ok -> log("Operation succeeded")
    :error -> log("Operation failed")
  end
end)
```

#### Fun Expression Properties:

- **First-class values**: Can be assigned, passed as arguments, and returned
- **Closures**: Capture variables from the enclosing scope
- **Pattern matching**: Support multiple clauses with pattern destructuring
- **Guards**: Optional guard expressions for conditional clause selection
- **Type safe**: Full integration with Lx's type system
- **Erlang compatible**: Compile to standard Erlang fun expressions

---

### 10. Guards

Conditions to refine pattern matching:

```lx
def is_non_empty_list(x) when is_list(x) andalso length(x) > 0 do
  :yes
end
```

Guards are pure boolean expressions. Avoid complex logic here.

---

### 11. Message Passing

Send messages to processes using the `!` operator:

```lx
# Basic message sending
pid ! {:log, "started"}

# Send with complex data
worker_pid ! {:task, %{id: 123, data: "process this"}}

# Send to multiple processes
for pid in worker_pids do
  pid ! {:broadcast, "message for all"}
end

# Message sending with precedence considerations
pid ! (a or b)          # Send the result of (a or b)
(pid ! a) or b          # Send a to pid, then OR with b
```

**Key Features:**
- **Fire-and-forget**: Returns the sent message, doesn't wait for response
- **Low precedence**: Message send has very low operator precedence
- **Any data**: Can send any Lx data structure (atoms, tuples, maps, etc.)
- **Process targeting**: Send to PIDs, registered names, or remote processes

**Precedence Note:**
The `!` operator has very low precedence (level 2), which means complex expressions often need parentheses:

```lx
# Without parentheses - may not behave as expected
pid ! a + b             # Equivalent to: (pid ! a) + b

# With parentheses - clearer intent
pid ! (a + b)           # Send the sum of a and b
```

**Generated Erlang Code:**
```lx
# Lx source
pid ! {:data, value}
```

```erlang
% Generated Erlang
Pid ! {data, Value}
```

---

### 12. Receive Expressions

Blocking pattern match for process messages with support for guards, multiple statements, and timeouts:

```lx
# Basic receive expression
receive do
  :ready -> proceed()
  {:data, data} -> handle(data)
  _ -> :unknown
end

# Receive with timeout
receive do
  :ready -> proceed()
  {:data, data} -> handle(data)
end after 5000 do
  :timeout
end

# Receive with guards
receive do
  {:message, msg} when is_binary(msg) -> handle_text(msg)
  {:message, msg} when is_integer(msg) -> handle_number(msg)
  {:error, reason} -> handle_error(reason)
  _ -> :unknown
end

# Receive with multiple statements in clause bodies
receive do
  {:echo, message, from} ->
    from ! {:response, message}
    log_message(message)
    :ok
  :stop ->
    cleanup_resources()
    save_state()
    "Server stopped"
  {:timeout, duration} ->
    set_timeout(duration)
    :timeout_set
  _ ->
    log_unknown_message()
    :error
end after 10000 do
  log_timeout()
  :receive_timeout
end

# Complex pattern matching in receive
receive do
  {:user_action, %{user_id: id, action: action}} when id > 0 ->
    process_user_action(id, action)
    update_user_stats(id)
    :processed
  {:system_event, [event_type | event_data]} ->
    handle_system_event(event_type, event_data)
    broadcast_event(event_type)
    :broadcasted
  _ ->
    :ignored
end
```

**Key Features:**
- **Selective Receive**: Only messages matching patterns are consumed
- **Pattern Matching**: Full pattern destructuring support
- **Guards**: Add conditional logic with `when` clauses
- **Multiple Statements**: Each clause can contain multiple statements
- **Timeout Support**: Use `after` clause for timeout handling
- **Variable Binding**: Variables bound in patterns are available in clause bodies
- **Message Queue**: Unmatched messages remain in the process mailbox

**Generated Erlang Code:**
```lx
# Lx source
receive do
  {:echo, msg, from} ->
    from ! {:response, msg}
    :ok
  :stop ->
    "stopped"
end after 5000 do
  :timeout
end
```

```erlang
% Generated Erlang
receive
    {echo, Msg, From} ->
        From ! {response, Msg},
        ok;
    stop ->
        "stopped"
after 5000 ->
    timeout
end
```

---

### 13. Control Flow

#### `if`/`else`:

The `if` expression evaluates a boolean condition and executes one of two branches. Both branches must return the same type, or the compiler will infer the most specific common type.

```lx
# Basic if/else
if flag do
  do_a()
else
  do_b()
end

# If without else (returns nil if condition is false)
if user_logged_in do
  "Welcome!"
end

# If with complex conditions
if x > 0 and y < 100 do
  "valid range"
else
  "invalid range"
end

# Nested if expressions
if temperature > 30 do
  if humidity > 80 do
    "hot and humid"
  else
    "hot and dry"
  end
else
  "comfortable"
end

# If with type inference
result = if score >= 90 do
  "excellent"    # string type inferred
else
  "good"        # same string type
end
# result has type: string()
```

**Type Inference Rules:**
- If both branches return the same type, that type is inferred
- If one branch returns a concrete type and the other is `nil`, the concrete type is inferred
- If branches return different types, the first branch's type takes precedence
- If without `else` has an implicit `else nil` branch

#### `if` with `case` (pattern matching):

```lx
if condition do
  success_value
case
  pattern1 -> handle_pattern1()
  pattern2 when guard -> handle_pattern2()
  _ -> default_case()
end
```

#### `with` expressions (elegant error handling):

The `with` expression provides elegant error handling and sequential pattern matching for operations that may fail. It compiles to nested Erlang `case` expressions for optimal performance.

##### Basic `with` expression:

```lx
# Simple with expression - executes body if pattern matches
def get_user_name(id) do
  with {1, 1} <- func_a() do
    "success"
  end
end
```

**Generated Erlang:**
```erlang
get_user_name(Id) ->
case func_a() of
    {1, 1} ->
        "success";
    Other ->
        Other
end.
```

##### `with` with `else` clause:

```lx
# With else clause for explicit error handling
def get_user_info(id) do
  with {1, 1} <- func_a(),
       {1, 2} <- func_b() do
    "all good"
  else
    "something failed"
  end
end
```

**Generated Erlang:**
```erlang
get_user_info(Id) ->
case func_a() of
    {1, 1} ->
        case func_b() of
            {1, 2} ->
                "all good";
            Other ->
                "something failed"
        end;
    Other ->
        "something failed"
end.
```

##### Multiple sequential bindings:

```lx
# Multiple bindings create nested case expressions
def complex_operation() do
  with x <- 1,
       y <- 2,
       z <- 3 do
    {x, y, z}
  end
end
```

**Generated Erlang:**
```erlang
complex_operation() ->
case 1 of
    X ->
        case 2 of
            Y ->
                case 3 of
                    Z ->
                        {X, Y, Z};
                    Other ->
                        Other
                end;
            Other ->
                Other
        end;
    Other ->
        Other
end.
```

##### `with` without else (auto-propagation):

```lx
# Without else clause, failures are automatically propagated
def simple_with() do
  with x <- get_value() do
    process(x)
  end
end
# If get_value() doesn't match x, the result is propagated as-is
```

##### Key Features:

- **Sequential Pattern Matching**: Each binding must succeed for the next to execute
- **Automatic Error Propagation**: Failed matches are propagated through `Other` variables when no `else` clause is present
- **Explicit Error Handling**: Use `else` clause to handle all failure cases uniformly
- **Efficient Compilation**: Generates optimal nested Erlang `case` expressions
- **Type Safety**: Full type checking for patterns, values, and success/failure branches
- **Variable Binding**: Variables bound in patterns are available in subsequent bindings and the success body
- **Erlang Compatibility**: Generated code follows Erlang conventions and integrates seamlessly with OTP

#### `case`:

Pattern matching with multiple clauses and support for guards:

```lx
# Basic case expression
case input do
  :ok -> handle_success()
  :error -> handle_error()
  _ -> fallback()
end

# Case with guards
case value do
  x when x > 0 -> "positive"
  x when x < 0 -> "negative"
  0 -> "zero"
  _ -> "unknown"
end

# Complex pattern matching
case data do
  {user, %{age: age}} when age >= 18 -> process_adult(user)
  {user, %{age: age}} when age < 18 -> process_minor(user)
  [head | tail] -> process_list(head, tail)
  %{status: :active, data: content} -> process_active(content)
  _ -> :unknown
end

# Case with multiple statements in clause bodies
case message do
  {:echo, msg, from} ->
    from ! {:response, msg}
    log_message(msg)
    :ok
  :stop ->
    cleanup()
    "Server stopped"
  _ ->
    log_unknown(message)
    :error
end
```

**Key Features:**
- **Pattern Matching**: Destructure data using patterns
- **Guards**: Add conditional logic with `when` clauses
- **Multiple Statements**: Each clause can contain multiple statements
- **Variable Binding**: Variables bound in patterns are available in clause bodies
- **Exhaustive Matching**: Use `_` for catch-all cases

#### `match ... rescue` (Error Handling):

The `match ... rescue` construct provides elegant error handling for operations that may fail. It supports both individual steps and sequential operations.

##### Individual Match Rescue Steps:

```lx
# Single match rescue - if pattern doesn't match, execute rescue expression
match {:ok, user} <- get_user(id) rescue error do
  {:user_not_found}
end
:continue_processing

# Multiple sequential match rescue steps
match {:ok, user} <- get_user(id) rescue error do
  {:user_error}
end
:log_user_retrieved
match {:ok, perms} <- get_permissions(user) rescue error do
  {:permission_error}
end
:log_permissions_retrieved
:success
```

##### Pattern Matching Support:

```lx
# Works with different pattern types
match [head | tail] <- get_list() rescue error do
  []
end
match %{name: user_name, age: user_age} <- get_user_data() rescue error do
  %{name: "unknown", age: 0}
end
match {:ok, value} <- computation() rescue error do
  {:error, "failed"}
end
```

##### Generated Erlang Code:

```lx
# Lx source
match {:ok, user} <- get_user() rescue result do
  {:error, result}
end
:continue
```

```erlang
% Generated Erlang
case get_user() of
    {ok, User} ->
        get_user();
    Result ->
        {error, Result}
end,
continue.
```

##### Sequential Match Rescue:

```lx
# Multiple steps create sequential case expressions
match {:ok, user} <- get_user() rescue result do
  {:user_error, result}
end
match {:ok, role} <- get_role(user) rescue result do
  {:role_error, result}
end
:success
```

```erlang
% Generated sequential Erlang cases
case get_user() of
    {ok, User} ->
        get_user();
    Result ->
        {user_error, Result}
end,
case get_role(User) of
    {ok, Role} ->
        get_role(User);
    Result ->
        {role_error, Result}
end,
success.
```

##### Key Features:

- **Type Safe**: Full type checking for patterns, values, and rescue expressions
- **Pattern Extraction**: Variables bound in patterns are available in subsequent code
- **Flexible Patterns**: Supports tuples, maps, lists, and complex nested patterns
- **Efficient Compilation**: Generates optimal nested Erlang case expressions
- **Sequential Processing**: Each step can depend on variables from previous steps


#### Simple Match Expressions

LX supports simple match expressions using the `match pattern <- expression` syntax for elegant pattern matching without rescue clauses:

```lx
# Basic simple match
def test_basic() do
  match {:ok, data} <- {:ok, 123}
  data
end

# List cons pattern matching
def test_cons() do
  match [h | t] <- [1, 2, 3]
  {h, t}
end

# Multiple sequential matches
def test_sequential() do
  match {:ok, user} <- get_user()
  match {:ok, perms} <- get_permissions(user)
  {user, perms}
end

# Tuple pattern matching
def test_tuple() do
  match {x, y, z} <- get_coordinates()
  x + y + z
end

# Atom pattern matching
def test_atom() do
  match :ok <- process_data()
  "success"
end
```

**Generated Erlang Code:**
```erlang
% Simple match compiles to case expression
test_basic() ->
case {ok, 123} of
    {ok, Data} ->
        Data;
    Other ->
        Other
end.

% Sequential matches create nested case expressions
test_sequential() ->
case get_user() of
    {ok, User} ->
        case get_permissions(User) of
            {ok, Perms} ->
                {User, Perms};
            Other ->
                Other
        end;
    Other ->
        Other
end.
```

**Key Features:**
- **Pattern Extraction**: Variables bound in patterns are available in subsequent code
- **Automatic Failure Handling**: Non-matching values are propagated through `Other` variables
- **Sequential Processing**: Multiple matches can be chained together
- **Type Safety**: Full type checking for patterns and expressions

#### `for` loop (List Comprehensions):

```lx
# Simple for loop
for x in list do
  x * 2
end

# For loop with guard
for x in list when x > 0 do
  x * 2
end

# Complex pattern matching in for loops
for %{name: name} = user in users when name != nil do
  user.age
end

# Named variable with pattern matching
for %{name: name1} = account in map_values(accounts) when name1 == name do
  true
end

# Multiple pattern types supported
for {key, value} in pairs when value > 10 do
  key
end

for [head | _tail] in lists do
  head
end
```

---

### 14. Data Structures

#### Tuples:

```lx
{user_id, count}, {}
```

#### Lists:

```lx
[1, 2, 3]
[head | tail]
```

Used for iteration and recursion.

##### List Pattern Matching:

```lx
# Simple list pattern
case list do
  [first, second, third] -> {first, second, third}
  [] -> :empty
  _ -> :unknown
end

# List cons pattern (head | tail)
case list do
  [head | tail] -> {head, tail}
  [] -> :empty
  _ -> :unknown
end

# Nested list patterns
case nested_list do
  [[a, b], [c, d]] -> {a, b, c, d}
  [head | [second | rest]] -> {head, second, rest}
  _ -> :unknown
end
```

#### Maps:

Maps are key-value data structures that provide efficient access and pattern matching capabilities.

##### Map Creation:

```lx
# Maps with atom keys (using colon syntax)
user = %{ name: "Alice", age: 30, active: true }

# Maps with general keys (using arrow syntax)
config = %{ "database_url" => "localhost", "port" => 5432 }

# Mixed key types
metadata = %{ :type => "user", "created_at" => "2023-01-01", 1 => "first" }

# Empty map
empty = %{}

# Complex maps with nested structures
complex = %{
  users: [%{ name: "Alice" }, %{ name: "Bob" }],
  config: %{ timeout: 5000 },
  metadata: {:version, "1.0.0"}
}
```

##### Map Access:

```lx
# Access with atom keys
name = user[:name]
age = user[:age]

# Access with general keys
url = config["database_url"]
port = config["port"]

# Access with variable keys
key = "port"
value = config[key]

# Nested map access
host = config[:database][:host]
```

##### Map Updates:

Maps support immutable updates using the `%{map | key: value}` syntax:

```lx
# Simple field update
updated_user = %{user | age: 31}

# Multiple field updates
updated_user = %{user | age: 31, status: "active"}

# Updates with general keys
updated_config = %{config | "timeout" => 10000}

# Updates with mixed key types
updated_data = %{data | :status => "active", "modified_at" => now()}

# Complex update patterns
def update_user_profile(user, changes) do
  %{user |
    profile: %{user[:profile] |
      updated_at: now(),
      changes: changes
    }
  }
end
```

**Generated Erlang Code:**
```lx
# LX source
updated_user = %{user | age: 31, status: "active"}
```

```erlang
% Generated Erlang
Updated_user = #{age => 31, status => "active" | User}
```

**Key Features:**
- **Immutable**: Original map is unchanged, returns a new map
- **Multiple Updates**: Can update multiple fields in a single expression
- **Mixed Key Types**: Supports atom, string, and other key types
- **Efficient**: Generates optimal Erlang map update syntax
- **Type Safety**: Updates are validated when possible

##### Key Tokens:

The Lx lexer automatically recognizes key tokens when an identifier is followed by a colon (`:`), creating a `KeyToken` that simplifies map creation with atom keys:

```lx
# Key token syntax (identifier:)
user = %{ name: "Alice", age: 30 }

# Equivalent to:
user = %{ :name => "Alice", :age => 30 }
```

**Key Token Features:**
- **Automatic recognition**: The lexer automatically detects `identifier:` as a key token
- **Atom conversion**: Key tokens are automatically converted to atom literals
- **Clean syntax**: Provides cleaner syntax for common atom keys
- **Compatibility**: Works seamlessly with pattern matching and map updates

##### Map Type Inference:

```lx
# Automatic type inference
def create_user(name, age) do
  %{name: name, age: age, active: true}
end
# Generates: -spec create_user(any(), any()) -> #{atom() => any()}.

# Maps with uniform types
def create_config() do
  %{"host" => "localhost", "port" => "5432"}
end
# Generates: -spec create_config() -> #{string() => string()}.
```

##### Map Pattern Matching:

```lx
# Pattern matching in function parameters
def process_user(%{ name: user_name, age: user_age }) do
  if user_age >= 18 do
    %{ name: user_name, status: :adult }
  else
    %{ name: user_name, status: :minor }
  end
end

# Pattern matching in case expressions
def handle_response(response) do
  case response do
    %{ status: :ok, data: data } -> data
    %{ status: :error, message: msg } -> {:error, msg}
    _ -> :unknown
  end
end

# Nested map patterns
data = %{ user: %{ name: "Alice", profile: %{ age: 25 } } }
%{ user: %{ name: user_name, profile: %{ age: user_age } } } <- data

# Map patterns with guards
def validate_user(user) do
  case user do
    %{ age: age, name: name } when age >= 18 -> %{ name: name, status: :adult }
    %{ age: age, name: name } when age < 18 -> %{ name: name, status: :minor }
    _ -> :invalid
  end
end
```

##### Key Types and Syntax:

- **Atom keys**: Use colon syntax `:key` or `key:` in patterns
- **String keys**: Use arrow syntax `"key" =>` in creation and patterns
- **Integer keys**: Use arrow syntax `1 =>` in creation and patterns
- **Mixed keys**: Can combine different key types in the same map

##### Pattern Matching Safety:

- **Default behavior**: Both `=` and `<-` operators allow mixed key types by default
- **Field validation**: Compiler validates that pattern fields exist in the map
- **Missing field errors**: Clear error messages when trying to match non-existent fields
- **Unsafe escape valve**: Use `unsafe` keyword to bypass all validations when needed

##### Unsafe Pattern Matching:

The `unsafe` keyword provides a complete escape valve for map pattern matching when you need to bypass all type checking and validation:

```lx
# When you're certain about the map structure but compiler can't verify it
unsafe %{ dynamic_field: value } = runtime_generated_map

# When working with external data that may have unknown fields
unsafe %{ required_field: data, optional_field: maybe_value } <- external_data

# When you need to handle maps with computed or variable keys
key = "computed_" ++ suffix
unsafe %{ key => value } <- dynamic_map
```

**Important**: `unsafe` completely disables type checking for that pattern match. Use it only when you're certain about the data structure and accept full responsibility for type correctness.

##### Generated Erlang Code:

```lx
# Lx source
user = %{ name: "Alice", age: 30 }
%{ name: user_name, age: _user_age } <- user
updated = %{user | age: 31}
name = user[:name]
```

```erlang
% Generated Erlang
User = #{name => "Alice", age => 30},
#{name := User_name, age := _} = User,
Updated = #{age => 31 | User},
Name = maps:get(name, User)
```

**Map Implementation Features:**
- **Map literals**: Use Erlang's native map syntax `#{key => value}`
- **Map access**: Compile to `maps:get(Key, Map)` function calls for safe access
- **Map updates**: Use Erlang's map update syntax `#{key => value | BaseMap}`
- **Pattern matching**: Use Erlang's map pattern syntax `#{key := Variable}`
- **Type safety**: Full integration with LX's type system and automatic spec generation
- **Key tokens**: Automatic conversion of `identifier:` to atom keys for cleaner syntax
- **Mixed keys**: Support for atom, string, integer, and other key types in the same map

**Practical Example:**
```lx
# Complete map workflow
def user_management_example() do
  # Create user with mixed key types
  user = %{
    :id => 1,
    "name" => "Alice",
    email: "alice@example.com",  # Key token syntax
    profile: %{age: 30, active: true}
  }

  # Access fields
  user_id = user[:id]
  user_name = user["name"]
  user_email = user[:email]

  # Pattern match
  %{:id => id, email: email, profile: %{age: age}} <- user

  # Update user
  updated_user = %{user |
    email: "alice.smith@example.com",
    profile: %{user[:profile] | age: 31}
  }

  {id, email, age, updated_user}
end
```

This generates clean, efficient Erlang code that integrates seamlessly with the BEAM VM and OTP patterns.

#### Binary/Bitstring Data:

Binaries are efficient data structures for handling raw binary data, protocol parsing, and network communication.

##### Binary Creation:

```lx
# Simple binary literals
empty = <<>>
bytes = <<1, 2, 3, 4>>
string_binary = <<"Hello World">>

# Binary with type specifiers
integer_bin = <<42/integer>>
float_bin = <<3.14159/float>>
binary_spec = <<"data"/binary>>
utf8_text = <<"Hello"/utf8>>
signed_int = <<42/signed>>
unsigned_int = <<42/unsigned>>

# Binary with size specifications
byte_value = <<255:8>>
word_value = <<1024:16>>
dword_value = <<16777216:32>>

# Combined size and type
sized_int = <<42:16/integer>>
sized_binary = <<"test":32/binary>>

# Complex binary construction
header = <<version:8, length:16, flags:8>>
packet = <<header/binary, "payload"/binary>>
```

##### Binary Pattern Matching:

```lx
# Basic binary pattern matching
data = <<42, 255:8, 1024:16, "hello"/binary>>

case data {
    <<first, second:8, third:16, rest/binary>> ->
        {first, second, third, rest}
    _ ->
        :error
}

# Advanced patterns with guards
case binary_data {
    <<first, second, rest/binary>> when first > 0 ->
        {:valid, first, second, rest}
    <<first/integer, _rest/binary>> ->
        {:invalid, first}
    _ ->
        :unknown
}

# Protocol parsing examples
case packet {
    <<"HTTP/", version:2/binary, " ", status:3/binary, rest/binary>> ->
        {:http, version, status, rest}
    <<"GET ", path/binary>> ->
        {:get, path}
    _ ->
        :unknown
}

# Binary patterns in function parameters
def parse_header(<<version:8, length:16, data:length/binary>>) do
  {:ok, version, length, data}
end

def parse_header(_) do
  :error
end

# Binary pattern matching in assignments
def extract_header(packet) do
  <<version:8, length:16, flags:8>> = packet
  {version, length, flags}
end
```

##### Binary Type Specifiers:

- **integer**: Signed integer (default size: 8 bits)
- **float**: Floating point number
- **binary**: Raw binary data
- **utf8**: UTF-8 encoded text
- **utf16**: UTF-16 encoded text
- **utf32**: UTF-32 encoded text
- **signed**: Signed integer
- **unsigned**: Unsigned integer
- **bits**: Bit-level data
- **bitstring**: Variable-length bit data

##### Size Specifications:

```lx
# Fixed sizes
<<value:8>>    # 8 bits
<<value:16>>   # 16 bits
<<value:32>>   # 32 bits

# Variable sizes
length = 10
<<data:length/binary>>

# Combined with types
<<number:32/integer>>
<<text:16/utf8>>
```

##### Practical Applications:

```lx
# Network protocol parsing
def parse_tcp_header(<<src_port:16, dst_port:16, seq:32, ack:32, rest/binary>>) do
  {:tcp_header, src_port, dst_port, seq, ack, rest}
end

# Message encoding
def encode_message(type_id, message) do
  case type_id do
    :text -> <<"TEXT"/binary, message/binary>>
    :data -> <<"DATA"/binary, message/binary>>
    :error -> <<"ERRO"/binary, message/binary>>
    _ -> <<"UNKN"/binary, message/binary>>
  end
end

# File format handling
def parse_png_chunk(<<length:32, type:4/binary, data:length/binary, crc:32>>) do
  {:png_chunk, type, data, crc}
end

# Binary assignment pattern matching
def process_packet(raw_data) do
  # Extract header information using binary patterns
  <<version:8, length:16, flags:8>> = raw_data

  # Use extracted variables
  if version == 1 do
    {:v1, length, flags}
  else
    {:unsupported, version}
  end
end

# Complex binary pattern extraction
def parse_network_frame(frame) do
  # Extract multiple fields in one pattern match
  <<preamble:32, header:64/binary, payload_size:16, payload:payload_size/binary, checksum:32>> = frame

  # All variables are now available for use
  {preamble, header, payload_size, payload, checksum}
end
```

---

### 15. Records

Structured data types with named fields for better code organization and type safety.

#### Record Definition:

Records are structured data types with named fields that provide type safety and efficient access patterns:

```lx
record Person {
    name :: string,
    age :: integer,
    email :: string
}

record Config {
    debug :: boolean,
    timeout :: integer,
    retries :: integer
}

# Records with complex types
record UserProfile {
    id :: integer,
    settings :: %{theme: string, language: string},
    permissions :: [string],
    metadata :: {string, any()}
}
```

**Generated Erlang Code:**
```erlang
-record(person, {name, age, email}).
-record(config, {debug, timeout, retries}).
-record(user_profile, {id, settings, permissions, metadata}).
```

**Key Features:**
- **Type Safety**: Field types are enforced at compile time
- **Efficient Access**: Generates optimal Erlang record operations
- **Named Fields**: Clear, self-documenting field names
- **Pattern Matching**: Full support for destructuring in patterns
- **Immutable Updates**: Safe field updates that return new records

#### Record Creation:

Records are created using the `RecordName{field: value, ...}` syntax:

```lx
# Basic record creation
person = Person{name: "Alice", age: 30, email: "alice@example.com"}
config = Config{debug: true, timeout: 5000, retries: 3}

# Records with complex field values
profile = UserProfile{
    id: 123,
    settings: %{theme: "dark", language: "en"},
    permissions: ["read", "write"],
    metadata: {"user_data", %{created_at: now()}}
}

# Records in function returns
def create_default_user(name) do
    Person{name: name, age: 0, email: ""}
end

# Records as function parameters
def process_user(user :: Person) do
    if user.age >= 18 do
        "adult"
    else
        "minor"
    end
end
```

**Generated Erlang Code:**
```lx
# LX source
person = Person{name: "Alice", age: 30, email: "alice@example.com"}
```

```erlang
% Generated Erlang
Person = #person{name = "Alice", age = 30, email = "alice@example.com"}
```

**Key Features:**
- **Type Validation**: Field types are checked at compile time
- **All Fields Required**: All record fields must be specified during creation
- **Efficient Creation**: Generates optimal Erlang record construction
- **Function Integration**: Records work seamlessly with function parameters and returns

#### Record Field Access:

Record fields are accessed using dot notation:

```lx
# Basic field access
name = person.name
age = person.age
is_debug = config.debug

# Field access in expressions
full_name = person.name ++ " (age: " ++ string_of_int(person.age) ++ ")"
timeout_seconds = config.timeout / 1000

# Field access in function calls
def greet_user(user) do
    "Hello, " ++ user.name ++ "!"
end

# Nested field access (when fields contain complex types)
def get_user_theme(profile) do
    profile.settings[:theme]  # Access map field within record
end

# Field access in guards (when supported)
def is_adult(person) when person.age >= 18 do
    true
end

def is_adult(_) do
    false
end
```

**Generated Erlang Code:**
```lx
# LX source
name = person.name
greeting = "Hello, " ++ person.name
```

```erlang
% Generated Erlang
Name = Person#person.name,
Greeting = "Hello, " ++ Person#person.name
```

**Key Features:**
- **Direct Access**: Efficient field access using Erlang record syntax
- **Type Safety**: Field access is validated against record definition
- **Expression Integration**: Field access works in any expression context
- **Guard Support**: Can be used in guard expressions for pattern matching

#### Record Updates (Immutable):

Records support immutable updates using the `RecordName{record | field: value}` syntax:

```lx
# Update single field
older_person = Person{person | age: 31}

# Update multiple fields
updated_person = Person{person | age: 31, email: "alice.smith@example.com"}

# Chained updates
final_person = Person{person | age: 31}
final_person2 = Person{final_person | email: "new@example.com"}

# Complex update example
def update_user_status(user, new_status) do
  User{user | status: new_status, last_updated: now()}
end
```

**Generated Erlang Code:**
```lx
# LX source
updated_person = Person{person | age: 31, email: "new@example.com"}
```

```erlang
% Generated Erlang
Updated_person = Person#{age => 31, email => "new@example.com"}
```

**Key Features:**
- **Type Safety**: Updates are validated against the record definition
- **Immutable**: Original record is unchanged, returns a new record
- **Multiple Fields**: Can update multiple fields in a single expression
- **Efficient**: Generates optimal Erlang map update syntax
- **Chainable**: Updates can be chained for complex transformations

#### Record Pattern Matching:

Records support comprehensive pattern matching in `case` expressions with full support for guards:

```lx
# Basic record pattern matching
case user do
  Person{name: "admin", age: _} -> :admin_user
  Person{age: age} when age >= 18 -> :adult
  Person{age: age} when age < 18 -> :minor
  _ -> :unknown
end

# Multiple field patterns with guards
case person do
  Person{name: "Eve", age: age} -> age
  Person{age: age} when age >= 18 -> age
  _ -> 0
end

# Complex pattern matching with multiple fields
case contact do
  Contact{name: name, email: email} when name != "" -> {name, email}
  Contact{phone: phone} when phone != "" -> {"unknown", phone}
  _ -> {"unknown", "unknown"}
end

# Pattern matching with boolean fields
case user do
  User{id: id, active: true} when id > 0 -> "active_user"
  User{id: id, active: false} when id > 0 -> "inactive_user"
  _ -> "invalid_user"
end
```

**Generated Erlang Code:**
```lx
# LX source
case person do
  Person{name: "Eve", age: age} -> age
  Person{age: age} when age >= 18 -> age
  _ -> 0
end
```

```erlang
% Generated Erlang
case Person of
    #person{name = "Eve", age = Age} -> Age;
    #person{age = Age} when Age >= 18 -> Age;
    _ -> 0
end
```

**Key Features:**
- **Pattern Extraction**: Variables bound in patterns are available in clause bodies
- **Guard Support**: Full support for `when` clauses with complex conditions
- **Field Selection**: Can match specific fields while ignoring others with `_`
- **Type Safety**: Compiler validates that pattern fields exist in the record definition
- **Erlang Compatibility**: Generates standard Erlang record patterns

#### Records in OTP Workers:

```lx
record UserState do
  users :: list,
  count :: integer
end

worker user_manager do
  def init(_) do
    {:ok, UserState{users: [], count: 0}}
  end

  def handle_call({:add_user, user}, _from, state) do
    new_users = [user | state.users]
    new_state = {state | users: new_users, count: state.count + 1}
    {:reply, :ok, new_state}
  end

  def handle_call(:get_count, _from, state) do
    {:reply, state.count, state}
  end
end
```

#### Maps in OTP Workers:

```lx
worker session_manager do
  def init(_) do
    # Initialize state as a map
    initial_state = %{ sessions: %{}, active_count: 0 }
    {:ok, initial_state}
  end

  def handle_call({:create_session, user_id}, _from, state) do
    # Extract current sessions and count using pattern matching
    %{ sessions: current_sessions, active_count: count } <- state

    # Create new session
    session_id = generate_session_id()
    session_data = %{ user_id: user_id, created_at: now(), active: true }

    # Update state with new session
    new_sessions = %{ session_id => session_data | current_sessions }
    new_state = %{ sessions: new_sessions, active_count: count + 1 }

    {:reply, {:ok, session_id}, new_state}
  end

  def handle_call({:get_session, session_id}, _from, state) do
    %{ sessions: sessions } <- state

    case sessions[session_id] do
      nil -> {:reply, {:error, :not_found}, state}
      session_data -> {:reply, {:ok, session_data}, state}
    end
  end

  def handle_call({:update_session, session_id, updates}, _from, state) do
    %{ sessions: sessions, active_count: count } <- state

    case sessions[session_id] do
      nil -> {:reply, {:error, :not_found}, state}
      session_data ->
        # Pattern match to extract session fields and apply updates
        %{ user_id: user_id, created_at: created_at } <- session_data
        updated_session = %{
          user_id: user_id,
          created_at: created_at,
          active: updates[:active],
          last_accessed: now()
        }
        new_sessions = %{ session_id => updated_session | sessions }
        new_state = %{ sessions: new_sessions, active_count: count }
        {:reply, :ok, new_state}
    end
  end
end
```

#### Records vs Maps: When to Use Each

LX provides both records and maps for structured data. Here's when to use each:

**Use Records When:**
- **Fixed Structure**: You have a well-defined, stable data structure
- **Type Safety**: You want compile-time field validation and type checking
- **Performance**: You need optimal field access performance
- **Documentation**: You want clear, self-documenting data structures
- **OTP Integration**: You're building OTP components (workers, supervisors)

**Use Maps When:**
- **Dynamic Structure**: You need flexible, runtime-determined fields
- **External Data**: You're working with JSON, APIs, or external data sources
- **Mixed Key Types**: You need different key types (atoms, strings, integers)
- **Partial Updates**: You frequently update subsets of fields
- **Interoperability**: You're interfacing with external Erlang/Elixir code

**Comparison Example:**
```lx
# Record approach - structured, type-safe
record User {
    id :: integer,
    name :: string,
    email :: string,
    active :: boolean
}

def create_user(name, email) do
    User{id: generate_id(), name: name, email: email, active: true}
end

def activate_user(user) do
    User{user | active: true}
end

# Map approach - flexible, dynamic
def create_user_map(name, email) do
    %{
        :id => generate_id(),
        "name" => name,
        "email" => email,
        :active => true,
        "metadata" => %{}
    }
end

def activate_user_map(user) do
    %{user | :active => true, "last_activated" => now()}
end

# Both approaches generate efficient Erlang code
# Records: #user{id = 1, name = "Alice", ...}
# Maps: #{id => 1, "name" => "Alice", ...}
```

**Best Practices:**
- **Start with Records**: Use records for core domain data structures
- **Use Maps for Configuration**: Maps work well for settings and configuration
- **Combine Both**: Records can contain map fields for flexible sub-structures
- **Consider Evolution**: Records are better for stable APIs, maps for evolving schemas

---

### 16. OTP Components

#### Workers:

```lx
worker my_worker do
  def init(_) do {:ok, []} end
  def handle_call(:get, _from, state) do {:reply, state, state} end
end
```

#### Supervisors:

```lx
supervisor top_sup {
  strategy one_for_one
  children [my_worker]
}
```

Use typed children blocks when needed:

```lx
children {
  worker [a]
  supervisor [b]
}
```

---

### 17. Specifications

Function contracts for static analysis and automatic spec generation.

#### Automatic Spec Generation

Lx automatically generates Erlang `-spec` declarations for all functions based on type annotations and type inference:

```lx
type number :: float | integer

def add(a :: integer, b :: number) do
  {a, b}
end
```

**Generated Erlang Code:**
```erlang
-type number() :: float() | integer().
-spec add(integer(), number()) -> {integer(), number()}.

add(A, B) ->
{A, B}.
```

**Spec Generation Features:**
- **Type Annotations**: Uses explicit type annotations from function parameters
- **Type Inference**: Automatically infers return types from function bodies
- **Type Aliases**: Properly uses defined type aliases (including `opaque` and `nominal`)
- **Positioning**: Specs are generated immediately above each function definition
- **Context Awareness**: Uses parameter context to infer variable types in return expressions
- **Smart Inference**: Automatically infers types for arithmetic operations, comparisons, and data structure operations

**Type Inference Examples:**
```lx
# Return type inferred as {integer(), number()}
def add(a :: integer, b :: number) do
  {a, b}
end

# Return type inferred as integer()
def multiply(x :: integer, y :: integer) do
  x * y
end

# Return type inferred as [integer()]
def double_list(numbers :: [integer]) do
  for n in numbers do
    n * 2
  end
end

# Return type inferred as boolean()
def is_positive(x :: integer) do
  x > 0
end

# Complex example with multiple functions and type aliases
type opaque user_id :: integer
type nominal celsius :: float

def create_user(name :: string, age :: integer) do
  id = generate_id()
  %{id: id, name: name, age: age}
end

def convert_to_celsius(fahrenheit :: float) do
  (fahrenheit - 32) * 5 / 9
end
```

**Generated Erlang Code:**
```erlang
-opaque user_id() :: integer().
-nominal celsius() :: float().

-spec create_user(string(), integer()) -> #{id := user_id(), name := string(), age := integer()}.
create_user(Name, Age) ->
    Id = generate_id(),
    #{id => Id, name => Name, age => Age}.

-spec convert_to_celsius(float()) -> celsius().
convert_to_celsius(Fahrenheit) ->
    (Fahrenheit - 32) * 5 / 9.
```

#### Manual Specifications

Function contracts for static analysis:

```lx
spec divide {
  requires y != 0
  ensures result * y == x
}
```

Helps enforce correctness at compile time.

---

### 18. Testing

Test declarations:

```lx
describe "math tests" {
  test "adds two" {
    assert 2 + 2 == 4
  }
}
```

Grouped and isolated via `describe`.

#### Test Utilities

The LX compiler includes utility functions for testing the complete compilation pipeline from LX source to Erlang output:

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

---

### 19. Application Definition

Defines metadata for runtime:

```lx
application {
  description "Analytics"
  vsn "0.1.0"
  applications [kernel, stdlib]
  registered [main]
  env [log: true]
}
```

---

### 20. Build System

Compilation and integration:

- Output: `.erl`, `.app.src`, `rebar.config`
- Uses Rebar3 if available (auto-download)
- `--skip-rebar` to disable external builds
- Compiled output: `_build/dev/` or `_build/prod/`
- Complies with standard Erlang project structure

---

### 21. Module System and Dependencies

Lx features a module and dependency system inspired by the Erlang/OTP ecosystem, enabling safe and explicit integration with external modules, compile-time type validation, and support for multiple dependency sources.

### 22. Error Handling and Suggestions

Lx provides comprehensive error handling with didactic suggestions to help users understand and fix common issues.

#### Error Categories

The compiler categorizes errors into several types:
- **Syntax Errors**: Invalid syntax, missing tokens, unexpected characters
- **Type Errors**: Type mismatches, invalid type conversions
- **Variable Errors**: Undefined variables, reassignment attempts, shadowing violations
- **Pattern Errors**: Invalid pattern matching, missing fields
- **Record Errors**: Invalid record operations, missing fields
- **Binary Errors**: Invalid binary specifications, size mismatches
- **Guard Errors**: Invalid guard expressions, unsupported operations
- **Dependency Errors**: Missing modules, invalid dependencies

#### Didactic Suggestions

When errors occur, the compiler provides practical suggestions with examples:

```lx
# Example: Invalid multi-clause function
def func do
  a
end

# Error message with suggestions:
# [Syntax Error] :1:1
# Multi-clause function must have at least one clause: ( for single-clause function or do for multi-clause function
#
# 💡 Suggestion:
#    For single-clause functions, add parentheses: def func() do ... end
#    For multi-clause functions, add at least one clause with parameters:
#      def func do
#        (x) -> x + 1
#        (y) -> y * 2
#      end
#    Multi-clause functions require at least one clause with parameters.

# Example: Variable scope error
def test() do
  y = x + 1  # x is not defined
end

# Error message with suggestions:
# [Variable Error] :2:5
# Variable 'x' is not defined
#
# 💡 Suggestion:
#    Variables must be defined before use. Check spelling or add an assignment: x = some_value
#    Make sure to declare all variables before using them in expressions.
```

#### Error Recovery

The parser implements robust error recovery:
- Continues parsing after encountering errors
- Reports all errors found in the code
- Provides context and source code highlighting
- Suggests fixes with practical examples

#### Declaring Dependencies

Dependencies can be declared in two ways:

##### 1. Global Configuration (lx.config)

```lx
project {
    name "my_project"
    version "1.0.0"
    deps [
        :erlang,
        :kernel,
        :stdlib,
        {:crypto, "~> 5.0.0"},
        {:mnesia, "~> 4.15.0"},
        {:custom_lib, :github, "user/repo"},
        {:local_lib, :path, "./libs/local_lib"}
    ]
    apps ["web_server", "database_worker", "auth_service"]
}
```

##### 2. Per-File Dependencies

```lx
# web_server.lx
deps [:cowboy, :jsx]

deps [
    :erlang,
    {:crypto, "~> 5.0.0"},
    {:custom_lib, :github, "user/repo"},
    {:local_lib, :path, "/path/to/lib"},
    {:hex_lib, :hex, "~> 2.1.0"}
]
```

#### Supported Dependency Sources
- **Simple**: `:erlang`, `:crypto` (built-in modules)
- **Versioned**: `{:crypto, "~> 5.0.0"}`
- **GitHub**: `{:custom_lib, :github, "user/repo"}`
- **Local path**: `{:local_lib, :path, "/path/to/lib"}`
- **Hex**: `{:hex_lib, :hex, "~> 2.1.0"}`

#### Dependency Resolution
- **Global**: The `lx.config` file defines global dependencies for the entire project
- **Local**: Each `.lx` file can declare additional dependencies or override versions
- **Priority**: Local dependencies override global ones with the same name

#### Integration with External Calls
- All `:module.function()` calls are validated at compile time
- Arity, argument types, and return types are checked using BEAM file information
- Clear errors for missing functions, arity, or type mismatches

#### Example Usage

```lx
deps [:erlang, :crypto]

worker cart_manager do
  def init(_) do
    timestamp = :erlang.system_time(:millisecond)
    uuid = :crypto.strong_rand_bytes(16)
    {:ok, #{carts: #{}, created_at: timestamp, session_id: uuid}}
  end
end
```

#### Benefits
- Type safety for all external integrations
- Explicit dependency management
- Support for umbrella and multi-app projects
- Full compatibility with OTP/Erlang

---

## Logical Operators

LX supports both symbolic and word forms for short-circuit logical operators:

| LX Syntax      | Description         | Erlang Output |
| -------------- | ------------------ | ------------- |
| a andalso b    | short-circuit AND  | a andalso b   |
| a && b         | short-circuit AND  | a andalso b   |
| a orelse b     | short-circuit OR   | a orelse b    |
| a || b         | short-circuit OR   | a orelse b    |

Both forms are fully equivalent in LX and always generate the modern Erlang/BEAM operators (`andalso`, `orelse`).

> Note: The classic `and`/`or` (non-short-circuit) are also available as `and`/`or` in LX, mapping to their respective Erlang forms.

---

## Recent Improvements

### Enhanced Expression Parsing

The LX compiler has been significantly improved with better expression parsing capabilities:

#### Unary Operators Support
- **Negative numbers**: Full support for unary minus operator (`-`) for negative numbers and expressions
- **Logical negation**: Support for unary `not` operator for boolean negation
- **Proper precedence**: Unary operators have correct precedence in the operator hierarchy
- **Expression integration**: Unary operators work seamlessly in complex expressions

**Examples:**
```lx
# Negative numbers
negative = -42
calculation = x * -1
complex_expr = a + (-b * c)

# Logical negation
flag = not condition
result = not (x > 0 and y < 10)

# In function bodies and expressions
def calculate(x) do
  if x > 0 do
    -x  # Negative of positive number
  else
    x * -2  # Multiply by negative two
  end
end
```

#### Operator Precedence System
- **Correct precedence hierarchy**: Assignment → Send → OR → AND → Comparison → Arithmetic → Unary → Function calls
- **Automatic parentheses**: The compiler automatically adds parentheses in generated Erlang code to preserve correct precedence
- **Complex expression support**: Handles nested expressions with mixed operators correctly
- **Unary integration**: Unary operators properly integrated into the precedence chain

#### Multi-Statement Case/Receive Bodies
- **Multiple statements**: Case and receive expressions now support multiple statements in clause bodies
- **Proper parsing**: Enhanced parser handles complex clause bodies with multiple expressions
- **Variable scoping**: Correct variable scoping within clause bodies
- **Pattern detection**: Improved pattern detection for complex case/receive clauses

#### Parser Architecture
- **Unified parsing**: Consistent parsing behavior between case and receive expressions
- **Better error handling**: Improved error messages for parsing issues
- **Robust parsing**: Enhanced parser recovery and error reporting
- **Unary expression parsing**: Complete implementation of unary expression parsing in both ExpressionParser and StatementParser

#### Code Generation Improvements
- **Unary code generation**: Proper Erlang code generation for unary expressions
- **Guard context**: Correct handling of unary operators in guard expressions
- **Type safety**: Full type checking and inference for unary expressions
- **Operator translation**: Accurate translation of LX unary operators to Erlang equivalents

**Example of enhanced parsing:**
```lx
# Complex case expression with multiple statements and unary operators
case message do
  {:process, data, callback} ->
    result = process_data(data)
    callback ! {:result, result}
    log_processing(data)
    :ok
  {:error, reason} ->
    log_error(reason)
    cleanup_resources()
    :error
  {:timeout, duration} when duration > 0 ->
    # Use negative timeout for retry
    retry_with_timeout(-duration)
    :retry
end

# Complex receive with precedence-aware message sending and unary operators
receive do
  {:echo, msg, from} ->
    from ! {:response, msg}
    :ok
  {:calculate, x, y} ->
    result = x + (-y * 2)  # Unary minus in complex expression
    :calculated
  {:negate, flag} ->
    opposite = not flag  # Unary not operator
    :negated
end

# Unary operators in arithmetic expressions
def complex_math(a, b, c) do
  # Multiple unary operators with correct precedence
  result = -a + (not b) * (-c)
  if not (result > 0) do
    -result
  else
    result
  end
end
```

**Generated Erlang Code:**
```lx
# LX source with unary operators
def test_unary(x) do
  negative = -42
  result = x * -1
  flag = not true
  {negative, result, flag}
end
```

```erlang
% Generated Erlang with proper unary operator handling
test_unary(X) ->
    Negative = -42,
    Result = X * -1,
    Flag = not true,
    {Negative, Result, Flag}.
```

#### Testing and Validation
- **Comprehensive tests**: Added extensive test coverage for unary operators
- **Edge cases**: Tests cover complex expressions, precedence, and integration scenarios
- **Regression testing**: Ensures existing functionality remains intact
- **Generated code validation**: Verifies that generated Erlang code compiles and runs correctly

These improvements ensure that LX code compiles correctly to Erlang while maintaining readable and predictable behavior, with complete support for unary operators in all contexts.

---
