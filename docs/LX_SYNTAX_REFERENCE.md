## Lx Language Syntax Reference

Lx is a statically scoped, expression-based functional language designed for building robust OTP applications that run on the Erlang/BEAM VM. It embraces immutable data, lightweight processes, and Erlang interoperability, with an ergonomic and expressive syntax.

---

### Table of Contents

1. Keywords — reserved words grouped by purpose
2. Operators and Punctuation — syntax markers and behavior
3. Literals — constants like numbers, strings, atoms
4. Identifiers — naming conventions for variables and modules
5. Comments — inline documentation syntax
6. Expressions — fundamental program building blocks
7. Pattern Matching — control flow via structural decomposition
8. Function Definitions — single and multi-clause declarations
8.1. Fun Expressions — anonymous functions with closures and higher-order support
9. Guards — conditional clauses in pattern matching
10. Message Passing — concurrent communication between processes
11. Receive Expressions — selective message handling
12. Control Flow — conditional constructs and loops
13. Data Structures — working with tuples and lists
14. Records — structured data with named fields
15. OTP Components — defining supervisors and workers
16. Specifications — declaring contracts for validation
17. Testing — structure and assertions for test blocks
18. Application Definition — project-level metadata and setup
19. Build System — compiling and generating artifacts
20. Module System and Dependencies — declaring dependencies and integrating with external modules
21. Error Handling and Suggestions — comprehensive error reporting with didactic examples

---

### 1. Keywords

These reserved words define control flow, type contracts, concurrency, and OTP structure. They cannot be redefined.

- **Core**: `def`, `defp`, `case`, `if`, `else`, `do`, `end`, `with`, `for`, `when`, `receive`, `after`, `true`, `false`, `nil`, `unsafe`
- **Data**: `record` — structured data type definitions
- **OTP**: `worker`, `supervisor`, `strategy`, `children`, `one_for_one`, `one_for_all`, `rest_for_one`
- **Specification**: `spec`, `requires`, `ensures`, `matches`
- **Testing**: `describe`, `test`, `assert`

---

### 2. Operators and Punctuation

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
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logic**: `and`, `or`, `not`, `andalso`, `orelse`
- **Grouping and data**: `()`, `{}`, `[]`
- **Block delimiters**: `do`, `end` — define code blocks for functions and control flow
- **Separators**: `,`, `;`

---

### 3. Literals

Immutable constant values available in source code:

- **Strings**: `"hello"`, supports escapes (`\n`, `\t`, `\r`, `\"`, `\\`)
- **Integers**: `42`, `-3`, `0`
- **Floats**: `3.14`, `-1.0`
- **Booleans**: `true`, `false`
- **Atoms (symbols)**: `:ok`, `:error`, `:timeout`
- **Nil/null value**: `nil`

#### String Literals

Strings are enclosed in double quotes and support escape sequences:

```lx
# Basic strings
message = "Hello, world!"

# Escape sequences
newline = "Line 1\nLine 2"
tab = "Column 1\tColumn 2"
quote = "He said \"Hello\""
backslash = "Path: C:\\Users\\Name"

# Invalid characters in strings
# "hello@world"  # Error: @ is not allowed in strings
```

**Supported escape sequences:**
- `\n` - newline
- `\t` - tab
- `\r` - carriage return
- `\"` - double quote
- `\\` - backslash

**Valid characters:** Only printable ASCII characters (32-126) are allowed in strings, excluding control characters and special symbols like `@`.

---

### 4. Identifiers

Naming conventions for program symbols:

- Variables: must start lowercase or with `_`, e.g., `count`, `_unused`
- Modules: lowercase, used in `:module.function()`
- Records: must start uppercase, e.g., `Person`, `UserData`
- Special: `__MODULE__` expands to the current module name

---

### 5. Comments

Inline documentation using `#`:

```lx
# This is a comment
value = 10  # Inline comment
```

No multiline comment syntax is supported.

---

### 6. Expressions

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

Code blocks are now defined using `do` and `end` keywords:

```lx
result = do
  a = 1
  b = 2
  a + b  # Last expression is returned
end
```

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

### 7. Pattern Matching

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

### 8. Function Definitions

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

### 8.1. Fun Expressions (Anonymous Functions)

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

### 9. Guards

Conditions to refine pattern matching:

```lx
def is_non_empty_list(x) when is_list(x) andalso length(x) > 0 do
  :yes
end
```

Guards are pure boolean expressions. Avoid complex logic here.

---

### 10. Message Passing

Send messages to processes using `!`:

```lx
pid ! {:log, "started"}
```

Returns the sent message. Useful for fire-and-forget.

---

### 11. Receive Expressions

Blocking pattern match for process messages:

```lx
receive do
  :ready -> proceed()
  {:data, D} -> handle(D)
end after 5000 do
  :timeout
end
```

Supports guards and default cases.

---

### 12. Control Flow

#### `if`/`else`:

```lx
if flag do
  do_a()
else
  do_b()
end
```

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

#### `with` expressions (monadic-style error handling):

The `with` expression provides elegant error handling and pattern matching for sequential operations that may fail.

##### `with`/`else`:

```lx
with {:ok, user} <= get_user(id) do
  user.name
else
  "Unknown user"
end
```

##### `with`/`case` (pattern matching):

```lx
with {:ok, user} <= get_user(id) do
  user.name
case
  {:error, :not_found} -> "User not found"
  {:error, reason} when reason == :timeout -> "Request timeout"
  {:error, _} -> "Unknown error"
  _ -> "Unexpected result"
end
```

##### Multiple `with` steps:

```lx
with {:ok, user} <= get_user(id),
     {:ok, role} <= get_role(user) do
  {user.name, role}
else
  {:error, "Failed to get user or role"}
end
```

##### `with` without else/case (returns optional):

```lx
with {:ok, user} <= get_user(id) do
  user.name
end
# Returns the value on success, or nil on failure
```

#### `case`:

```lx
case input do
  :ok -> handle()
  _ -> fallback()
end
```

#### `match ... rescue` (Error Handling):

The `match ... rescue` construct provides elegant error handling for operations that may fail. It supports both individual steps and sequential operations.

##### Individual Match Rescue Steps:

```lx
# Single match rescue - if pattern doesn't match, execute rescue expression
match {:ok, user} <- get_user(id) rescue { :user_not_found }
:continue_processing

# Multiple sequential match rescue steps
match {:ok, user} <- get_user(id) rescue { :user_error }
:log_user_retrieved
match {:ok, perms} <- get_permissions(user) rescue { :permission_error }
:log_permissions_retrieved
:success
```

##### Pattern Matching Support:

```lx
# Works with different pattern types
match [head | tail] <- get_list() rescue { [] }
match %{name: user_name, age: user_age} <- get_user_data() rescue { %{name: "unknown", age: 0} }
match {:ok, value} <- computation() rescue { {:error, "failed"} }
```

##### Generated Erlang Code:

```lx
# Lx source
match {:ok, user} <- get_user() rescue { :error }
:continue
```

```erlang
% Generated Erlang
case get_user() of
    {ok, User} ->
        continue;
    _ ->
        error
end
```

##### Sequential Match Rescue:

```lx
# Multiple steps create nested case expressions
match {:ok, user} <- get_user() rescue { :user_error }
match {:ok, role} <- get_role(user) rescue { :role_error }
:success
```

```erlang
% Generated nested Erlang cases
case get_user() of
    {ok, User} ->
        case get_role(User) of
            {ok, Role} ->
                success;
            _ ->
                role_error
        end;
    _ ->
        user_error
end
```

##### Key Features:

- **Type Safe**: Full type checking for patterns, values, and rescue expressions
- **Pattern Extraction**: Variables bound in patterns are available in subsequent code
- **Flexible Patterns**: Supports tuples, maps, lists, and complex nested patterns
- **Efficient Compilation**: Generates optimal nested Erlang case expressions
- **Sequential Processing**: Each step can depend on variables from previous steps

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

### 13. Data Structures

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
```

```erlang
% Generated Erlang
User = #{name => "Alice", age => 30},
#{name := User_name, age := _} = User
```

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

### 14. Records

Structured data types with named fields for better code organization and type safety.

#### Record Definition:

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
```

#### Record Creation:

```lx
person = Person{name: "Alice", age: 30, email: "alice@example.com"}
config = Config{debug: true, timeout: 5000, retries: 3}
```

#### Record Field Access:

```lx
name = person.name
age = person.age
is_debug = config.debug
```

#### Record Updates (Immutable):

```lx
# Update single field
older_person = {person | age: 31}

# Update multiple fields
updated_person = {person | age: 31, email: "alice.smith@example.com"}

# Chained updates
final_person = {person | age: 31}
final_person2 = {final_person | email: "new@example.com"}
```

#### Records in Pattern Matching:

```lx
case user {
  Person{name: "admin", age: _} -> :admin_user
  Person{age: age} when age >= 18 -> :adult
  Person{age: age} when age < 18 -> :minor
  _ -> :unknown
}
```

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

---

### 15. OTP Components

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

### 16. Specifications

Function contracts for static analysis:

```lx
spec divide {
  requires y != 0
  ensures result * y == x
}
```

Helps enforce correctness at compile time.

---

### 17. Testing

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

### 18. Application Definition

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

### 19. Build System

Compilation and integration:

- Output: `.erl`, `.app.src`, `rebar.config`
- Uses Rebar3 if available (auto-download)
- `--skip-rebar` to disable external builds
- Compiled output: `_build/dev/` or `_build/prod/`
- Complies with standard Erlang project structure

---

### 20. Module System and Dependencies

Lx features a module and dependency system inspired by the Erlang/OTP ecosystem, enabling safe and explicit integration with external modules, compile-time type validation, and support for multiple dependency sources.

### 21. Error Handling and Suggestions

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
