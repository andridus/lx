# Task 1 Examples

This directory contains examples for Task 1: Functions with Literals.

## Files

### `simple.lx`
The most basic example - a single function returning an integer literal.

**Usage:**
```bash
cd lx1
v run . ../examples/task_01/simple.lx
```

**Expected Output:**
```erlang
-module(main).
-export([answer/0]).

answer() ->
    42.
```

### `multiple.lx`
Example with multiple functions returning different types of literals.

**Usage:**
```bash
cd lx1
v run . ../examples/task_01/multiple.lx
```

**Expected Output:**
```erlang
-module(main).
-export([answer/0, greeting/0, pi/0, is_active/0, is_inactive/0, status/0, error_status/0, empty/0]).

answer() ->
    42.

greeting() ->
    <<"Hello, World!"/utf8>>.

pi() ->
    3.14159.

is_active() ->
    true.

is_inactive() ->
    false.

status() ->
    ok.

error_status() ->
    error.

empty() ->
    nil.
```

### `literals.lx`
Comprehensive example showing all supported literal types with comments.

**Usage:**
```bash
cd lx1
v run . ../examples/task_01/literals.lx
```

This example demonstrates:
- Integer literals (small, large, zero)
- Float literals (pi, small decimals, precise values)
- String literals (simple, with spaces, with escapes, empty)
- Boolean literals (true, false)
- Atom literals (common atoms, custom atoms)
- Nil literal

## Supported Syntax (Task 1)

```lx
def function_name() do
    literal_value
end
```

## Supported Literals

| Type | Examples | Erlang Output |
|------|----------|---------------|
| Integer | `42`, `0`, `1000000` | `42`, `0`, `1000000` |
| Float | `3.14`, `0.1`, `2.718` | `3.14`, `0.1`, `2.718` |
| String | `"Hello"`, `"World!"` | `<<"Hello"/utf8>>`, `<<"World!"/utf8>>` |
| Boolean | `true`, `false` | `true`, `false` |
| Atom | `:ok`, `:error`, `:timeout` | `ok`, `error`, `timeout` |
| Nil | `nil` | `nil` |

## Testing the Examples

You can compile and test these examples using the Erlang compiler:

```bash
# Compile LX to Erlang
cd lx1
v run . ../examples/task_01/simple.lx > simple.erl

# Compile with Erlang
erlc simple.erl

# Test in Erlang shell
erl -noshell -eval "io:format('~p~n', [main:answer()])." -s init stop
```

## Restrictions (Task 1)

- ✅ Functions with no parameters only
- ✅ Single literal as function body
- ❌ No variables or expressions
- ❌ No function calls
- ❌ No control flow (if, case, etc.)
- ❌ No data structures (lists, tuples, maps)
- ❌ No function parameters

These restrictions will be lifted in subsequent tasks.