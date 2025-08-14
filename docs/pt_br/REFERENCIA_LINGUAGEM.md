# LX Language Reference

## Overview

LX is a functional language that compiles to Erlang. It provides:
- Pattern-matched multi-clause functions with guards
- A strong type system with type annotations, custom types, and automatic spec generation
- Records, lists, tuples, maps, binaries/bitstrings
- Anonymous functions (lambdas), control-flow expressions (if, case, with, match)
- Concurrency primitives (spawn, send, receive)
- List comprehensions

Everything compiles to readable Erlang with `-spec` annotations.


## Project Creation and CLI

Use the `lx` CLI.

```bash
# Create a new umbrella-style project (apps/ + <project>.yml)
lx new my_project

# Inside the project, add a new app
cd my_project
lx add app my_app

# Compile the whole project (generates _build/ umbrella with rebar3 files)
lx .

# Compile a file or a directory (non-project mode)
lx compile path/to/file.lx
lx compile path/to/dir

# Run a single file (lightweight: erlc + erl)
lx run path/to/file.lx

# Open rebar3 shell for the project (precompiles first)
lx shell [dir]

# Create a global symlink to current lx binary
sudo lx symlink [--force]
```

Project layout created by `lx new <name>`:

```
<project>/
  apps/
  <project>.yml           # project configuration for build (erl_opts, deps)
  _build/                 # generated umbrella (rebar3, apps/*)
```

Notes:
- `<project>.yml` is used to produce `_build/rebar.config` and `config/sys.config`.
- For each app under `apps/<app>/`, sources are compiled into `_build/apps/<app>/src`.
- The application descriptor `<app>.app.src` is generated/ensured in `_build`.


## Syntax Basics

### Comments
```lx
# single line comment
```

### Identifiers
- Variables and functions: snake_case
- Record names: snake_case (maps to Erlang `-record(name, ...)`)
- Module name is implicitly the file name

### Literals
```lx
# Numbers
42
3.14

# Strings (compile to UTF-8 binaries)
"hello"

# Booleans
true
false

# Nil
nil

# Atoms
:ok
:error
:timeout
```


## Collections and Data

### Lists
```lx
[]
[1, 2, 3]

# Cons and concatenation
[0 | [1, 2]]     # [0, 1, 2]
[1, 2] ++ [3, 4] # [1, 2, 3, 4]

# Pattern matching
[head | tail] = [1, 2, 3]
```

### Tuples
```lx
{1, 2}
{:ok, "ok"}

{status, msg} = {:error, "oops"}
```

### Maps
Implemented with creation, access, update and pattern matching.
```lx
user = %{ name: "John", age: 30 }
name = user.name       # dot access
age  = user[:age]      # index access

user2 = %{ user | age: 31 }             # update
user3 = %{ user | email: "john@x.com" } # add key

# pattern matching in maps
auth = case user do
  %{name: n} -> {:ok, n}
  _ -> {:error}
end
```


## Binaries and Bitstrings

Both expressions and pattern matching support sizes and qualifiers (endianness, signedness, types).

### Expressions
```lx
<<>>
<<1, 2, 3>>

value = 0x1234
big    = <<value:16/big>>
little = <<value:16/little>>

int_val = 42
float_val = 3.14
bin = <<int_val:32/integer, float_val:64/float>>

data = "hello"
chunk = <<data/binary>>
```

### Pattern Matching
```lx
def parse_header(packet) do
  <<version:8, size:16, rest/binary>> = packet
  {version, size, rest}
end

# Mixed options and variable-size segment
def decode(packet) do
  <<typ:4, _rsv:4, id:16/big, sz:32/big, payload:sz/binary, _/binary>> = packet
  {typ, id, payload}
end
```


## Records

Define records with typed fields:
```lx
record user { name :: string, age :: integer }

u = user{name: "John", age: 30}
u.name           # field access

# Pattern matching
def who(u) do
  case u do
    user{name: n, age: a} when a > 18 -> n
    _ -> "minor"
  end
end
```

Generated Erlang uses `-record(user, ...)` and `#user{...}`.


## Functions

### Definitions
```lx
# public function
def add(a :: integer, b :: integer) do
  a + b
end

# private function
defp helper(x :: integer) do
  x + 1
end
```

### Multiple Clauses and Guards
```lx
def classify do
  (n) when n > 0 -> :positive
  (n) when n < 0 -> :negative
  (_) -> :zero
end

# pattern-based dispatch
def process_list do
  ([]) -> "empty"
  ([h | _]) -> "non_empty"
end
```

### Return Type Annotation (on multi-clause blocks)
```lx
def countdown :: string do
  (0) -> "done"
  (n :: integer) -> countdown(n - 1)
end
```

### Type Specs
- Types are inferred and emitted as `-spec` in Erlang.
- Parameter annotations with `::` influence the generated spec.
- Multi-clause functions may produce union return types.


## Anonymous Functions (Lambdas)

Single-line and multi-line forms, including multi-head lambdas. Invocation uses `.(...)`.
```lx
# single-line
def demo() do
  f = fn(x :: integer, y :: integer) -> x + y
  f.(3, 4)
end

# multi-line do/end
def demo2() do
  g = fn(x :: integer) do
    y = x * 2
    y + 1
  end
  g.(10)
end

# multi-head
def demo3() do
  h = fn do
    (:ok) -> "success"
    (:error) -> "failure"
    (_) -> "unknown"
  end
  h.(:ok)
end
```

Limitations:
- Anonymous functions are not recursive (self calls are rejected in current implementation).


## Control Flow

### If
```lx
def test_if(x) do
  if x > 0 do
    "positive"
  else
    "not positive"
  end
end

# If without else returns nil in the false branch
```

### Case (with patterns and guards)
```lx
def handle(result) do
  case result do
    {:ok, data} -> data
    {:error, reason} -> reason
    _ -> "unknown"
  end
end
```

### With expression
```lx
def test_with() do
  result = {:success, 10}
  with {:success, x} <- result do
    x
  else
    {:error, _} -> 0
  end
end
```

### Match and Match..Rescue
```lx
# match propagates non-matching values
def test_simple_match() do
  data = {:ok, "success"}
  match {:ok, value} <- data
  value
end

# rescue on mismatch
def test_match_rescue() do
  data = {:error, "failed"}
  match {:ok, res} <- data rescue err do
    {:failed, err}
  end
  :done
end
```


## Concurrency

Supported primitives:
- `spawn(fn() -> ... end)` – spawns a process
- Send operator `!`
- `receive do ... end`

```lx
def server_loop() do
  :ok
end

def start() do
  pid = spawn(fn() -> server_loop() end)
  pid ! {:message, "hello"}
end


def wait() do
  receive do
    {:message, data} -> data
    :stop -> :ok
  end
end
```


## List Comprehensions

Single-generator comprehensions with optional guard and transformation. Nesting is supported by nesting `for` blocks.
```lx
def squares() do
  numbers = [1, 2, 3, 4]
  for x in numbers do
    x * x
  end
end

# with filter
def filtered() do
  numbers = [1, 2, 3, 4, 5]
  for x in numbers when x > 2 do
    x
  end
end

# nested
def nested() do
  matrix = [[1, 2], [3, 4]]
  for row in matrix do
    for x in row do
      x + 1
    end
  end
end

# membership
def membership() do
  numbers = [1, 2, 3, 4, 5]
  allowed = [2, 4]
  for x in numbers when x in allowed do
    x * 10
  end
end
```


## Types

### Built-in Types
- integer, float, boolean, binary (string), atom, list(T), tuple(...), map(K, V), function

### Type Annotations
```lx
def add(a :: integer, b :: integer) :: integer do
  a + b
end
```

### Custom Types
```lx
# Simple alias
type user_id :: integer

def id(x :: user_id) do
  x
end

# Opaque type
type opaque user_id :: integer

# Nominal type
type nominal email :: string
```

These generate corresponding Erlang type declarations (including `-opaque` and a nominal tag).


## Directives

- `@doc "Text"` – emits a module-level `-doc` attribute for the next public function
- `$print(expr)` – compile-time inspection of an expression (removed from output)
- `$type(expr)` – ensures and records the inferred type in the generated spec

Examples:
```lx
@doc "Adds two numbers"
def add(a :: integer, b :: integer) do
  $print(a)
  $type(a + b)
  a + b
end
```

Unknown directives or wrong arity produce errors.


## Operators and Semantics

- Arithmetic: `+ - * /`
- Comparison: `== != < <= > >=` (`!=` compiles to Erlang `/=`)
- Boolean: `and` / `or` (compile to Erlang `andalso` / `orelse`)
- Bitwise: `&&& ||| ^^^ <<< >>>` (compile to `band bor bxor bsl bsr`)

Operator precedence follows Erlang semantics.


## Application Block, Imports and Deps

The language supports an `application { ... }` block at file top. Its content is currently emitted as Erlang comments for documentation and tooling.
```lx
application {
  description: "My App",
  vsn: "0.1.0",
  deps: [:cowboy, :jsx],
  registered: [:main_server],
  env: %{debug: true, port: 8080}
}
```

`import :module` is accepted and currently emitted as a comment (`%% Import: module`).

Dependency resolution and runtime linking are managed by the project build (rebar3) generated by the CLI from `<project>.yml`. The in-source `deps` and `import` act as metadata today and do not link code by themselves.


## Limitations and Notes

- Anonymous functions cannot be recursive.
- `import` and `application.deps` are metadata in source; actual dependency management is handled by the CLI-generated rebar3 umbrella under `_build/`.
- If without an `else` returns `nil` in the false branch.
- Strings are UTF-8 binaries.


## Style

- Variables and functions: `snake_case`
- Records: `snake_case`
- Atoms: `:lowercase` or `:snake_case`
- Keep functions small and prefer multiple clauses with pattern matching and guards.


## Examples

### Multi-clause with types
```lx
def factorial :: integer do
  (0) -> 1
  (n :: integer) -> n * factorial(n - 1)
end
```

### Binary encode/decode
```lx
def encode(typ, id, payload) do
  size = byte_size(payload)
  <<typ:4, 0:4, id:16/big, size:32/big, payload/binary>>
end

def decode(packet) do
  <<typ:4, _rsv:4, id:16/big, sz:32/big, data:sz/binary, _/binary>> = packet
  {typ, id, data}
end
```

### With + match
```lx
def example(maybe_data) do
  with {:success, data} <- maybe_data do
    data
  else
    _ -> "failed"
  end
end
```

This document reflects the features validated by the current test suite and CLI behavior. As the language evolves, sections will be extended accordingly.
