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

---

### 1. Keywords

These reserved words define control flow, type contracts, concurrency, and OTP structure. They cannot be redefined.

- **Core**: `fun`, `pub`, `case`, `if`, `else`, `for`, `when`, `receive`, `after`, `true`, `false`, `nil`
- **Data**: `record` — structured data type definitions
- **OTP**: `worker`, `supervisor`, `strategy`, `children`, `one_for_one`, `one_for_all`, `rest_for_one`
- **Specification**: `spec`, `requires`, `ensures`, `matches`
- **Testing**: `describe`, `test`, `assert`

---

### 2. Operators and Punctuation

Syntax symbols used for operations, declarations, and structure:

- **Assignment**: `=` — bind a value to a variable once
- **Pattern matching**: `<-` — explicit pattern matching operator (recommended for maps)
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
- **Grouping and data**: `()`, `{}`, `.{}`, `[]`
- **Separators**: `,`, `;`

---

### 3. Literals

Immutable constant values available in source code:

- Strings: `"hello"`, supports escapes (`\n`, `\t`, etc.)
- Integers: `42`, `-3`, `0`
- Floats: `3.14`, `-1.0`
- Booleans: `true`, `false`
- Atoms (symbols): `:ok`, `:error`, `:timeout`
- Nil/null value: `nil`

---

### 4. Identifiers

Naming conventions for program symbols:

- Variables: must start lowercase or with `_`, e.g., `count`, `_unused`
- Modules: lowercase, used in `mod.fun()`
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

#### Block expressions:

```lx
result = {
  a = 1
  b = 2
  a + b  # Last expression is returned
}
```

#### Function calls:

```lx
sum(3, 4)
math.pow(2, 8)
```

---

### 7. Pattern Matching

Used for destructuring and conditional logic based on shape:

```lx
case msg {
  :ok -> handle_ok()
  [head | tail] -> handle_list(head, tail)
  .{x, y} -> sum(x, y)
  %{ name: user_name, age: user_age } -> process_user(user_name, user_age)
  _ -> :default
}
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

#### Single clause:

```lx
fun greet(name) { "Hello, " ++ name }
pub fun ping() { :pong }
```

#### Multiple clauses:

```lx
fun factorial {
  (0) { 1 }
  (N) when N > 0 { N * factorial(N - 1) }
}
```

---

### 9. Guards

Conditions to refine pattern matching:

```lx
fun is_non_empty_list(x) when is_list(x) andalso length(x) > 0 {
  :yes
}
```

Guards are pure boolean expressions. Avoid complex logic here.

---

### 10. Message Passing

Send messages to processes using `!`:

```lx
pid ! .{:log, "started"}
```

Returns the sent message. Useful for fire-and-forget.

---

### 11. Receive Expressions

Blocking pattern match for process messages:

```lx
receive {
  :ready -> proceed()
  .{:data, D} -> handle(D)
} after 5000 {
  :timeout
}
```

Supports guards and default cases.

---

### 12. Control Flow

#### `if`/`else`:

```lx
if flag {
  do_a()
} else {
  do_b()
}
```

#### `case`:

```lx
case input {
  :ok -> handle()
  _ -> fallback()
}
```

#### `for` loop:

```lx
for X in list when X > 0 {
  double(X)
}
```

---

### 13. Data Structures

#### Tuples:

```lx
.{user_id, count}, .{}
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
  metadata: .{:version, "1.0.0"}
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
# Basic pattern matching with assignment operator (=) or pattern operator (<-)
%{ name: user_name, age: user_age } = user
%{ name: user_name, age: _user_age } <- user

# Mixed key pattern matching
%{ :status => status, "message" => message } <- response

# Partial pattern matching (extract subset of fields)
%{ name: user_name } <- user

# Pattern matching in function parameters
pub fun process_user(%{ name: user_name, age: user_age }) {
  if user_age >= 18 {
    %{ name: user_name, status: :adult }
  } else {
    %{ name: user_name, status: :minor }
  }
}

# Pattern matching in case expressions
pub fun handle_response(response) {
  case response {
    %{ status: :ok, data: data } -> data
    %{ status: :error, message: msg } -> .{:error, msg}
    _ -> :unknown
  }
}

# Nested map patterns
data = %{ user: %{ name: "Alice", profile: %{ age: 25 } } }
%{ user: %{ name: user_name, profile: %{ age: user_age } } } <- data

# Map patterns with guards
pub fun validate_user(user) {
  case user {
    %{ age: age, name: name } when age >= 18 -> %{ name: name, status: :adult }
    %{ age: age, name: name } when age < 18 -> %{ name: name, status: :minor }
    _ -> :invalid
  }
}
```

##### Key Types and Syntax:

- **Atom keys**: Use colon syntax `:key` or `key:` in patterns
- **String keys**: Use arrow syntax `"key" =>` in creation and patterns
- **Integer keys**: Use arrow syntax `1 =>` in creation and patterns
- **Mixed keys**: Can combine different key types in the same map

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
record UserState {
    users :: list,
    count :: integer
}

worker user_manager {
  fun init(_) {
    .{:ok, UserState{users: [], count: 0}}
  }

  fun handle_call(.{:add_user, user}, _from, state) {
    new_users = [user | state.users]
    new_state = {state | users: new_users, count: state.count + 1}
    .{:reply, :ok, new_state}
  }

  fun handle_call(:get_count, _from, state) {
    .{:reply, state.count, state}
  }
}
```

#### Maps in OTP Workers:

```lx
worker session_manager {
  fun init(_) {
    # Initialize state as a map
    initial_state = %{ sessions: %{}, active_count: 0 }
    .{:ok, initial_state}
  }

  fun handle_call(.{:create_session, user_id}, _from, state) {
    # Extract current sessions and count using pattern matching
    %{ sessions: current_sessions, active_count: count } <- state

    # Create new session
    session_id = generate_session_id()
    session_data = %{ user_id: user_id, created_at: now(), active: true }

    # Update state with new session
    new_sessions = %{ session_id => session_data | current_sessions }
    new_state = %{ sessions: new_sessions, active_count: count + 1 }

    .{:reply, .{:ok, session_id}, new_state}
  }

  fun handle_call(.{:get_session, session_id}, _from, state) {
    %{ sessions: sessions } <- state

    case sessions[session_id] {
      nil -> .{:reply, .{:error, :not_found}, state}
      session_data -> .{:reply, .{:ok, session_data}, state}
    }
  }

  fun handle_call(.{:update_session, session_id, updates}, _from, state) {
    %{ sessions: sessions, active_count: count } <- state

    case sessions[session_id] {
      nil -> .{:reply, .{:error, :not_found}, state}
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
        .{:reply, :ok, new_state}
    }
  }
}
```

---

### 15. OTP Components

#### Workers:

```lx
worker my_worker {
  fun init(_) { .{:ok, []} }
  fun handle_call(:get, _from, state) { .{:reply, state, state} }
}
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
