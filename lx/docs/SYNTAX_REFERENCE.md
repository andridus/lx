# LX1 Syntax Reference

This document describes the LX1 language syntax as implemented in the current codebase (parser, lexer, analyzer and Erlang generator) and validated by the test suite.

The module name is the file name without extension. Public functions are declared with `def`; private functions use `defp` (token exists only in docs; in lx1 only `def` is parsed).

## Lexical elements

- Comments: start with `#` and go until end of line.
- Newlines and `;` are statement separators in blocks.
- Identifiers: `snake_case` for variables/functions, `PascalCase` for record names. Atoms use a leading colon `:atom`.
- Literals: integer, float, string, boolean (`true`/`false`), atom (`:ok`), `nil`.
- Operators (infix, precedence via kernel): `+ - * / == != < <= > >= and or &&& ||| ^^^ <<< >>>` and the send operator `!`.
- Delimiters: `() [] {} %{} << >>`.

Notes:
- Not-equal is written `!=`.
- Strings are emitted as Erlang binaries. Escape sequences: `\n \t \r \\" \\` are supported in the lexer.

## Top-level forms

- Records
- Type definitions (nominal and opaque aliases, parameterized types)
- Functions (single-body and multi-head)
- Declarations for deps/import/application (module system metadata)
- Test blocks (`describe`/`test`) and documentation directives (`@moduledoc`, `@doc`)

### Records

Definition:
```lx
record Person {
  name :: string
  age :: integer
}
```
- Fields may have default values, optionally followed by a type: `name = "John" :: string`.
- Without defaults, `:: type` is mandatory.

Construction and access:
```lx
p = Person{name: "John", age: 30}
p.name
```

Update (record update syntax):
```lx
%Person{p | age: 31}
```

### Types

Type definitions:
```lx
# Simple alias
type user_id :: integer

# Opaque alias
type opaque user_id :: integer

# Nominal alias
type nominal email :: string

# Parameterized aliases
type box(T) :: {T}
```
- Generic parameter list: `name(T, U, ...)`.
- Function parameters and pattern positions support `:: type` annotations.

Note: Union types with `|` are not parsed yet in lx1 (see Missing Features).

### Functions

Single-body function with parameters:
```lx
def sum(a :: integer, b :: integer) do
  a + b
end
```

Multi-head function (pattern clauses):
```lx
def classify do
  (0) -> :zero
  (n) when n > 0 -> :positive
end
```
- A `def` without parameter parentheses opens a multi-head section where each head is `(pattern) -> body`.
- Guards use `when` followed by any expression.

Function calls:
```lx
sum(1, 2)
+(1, 2)
```

### Variables and bindings

- Binding: `x = expr`.
- Rebinding the same name in the same scope is rejected by analysis (per tests).
- Variable references are simple identifiers.

Pattern bindings (left side as pattern):
```lx
[head | tail] = [1,2,3]
{ok, value} = call()
<<v:8, rest/binary>> = packet
```

### Expressions

- Literals: integers, floats, strings, booleans, atoms, `nil`.
- Lists: `[]`, `[1, 2, 3]`, `[h | t]`.
- Tuples: `{}`, `{1, 2}`, nested allowed.
- Maps: `%{key: value, other: 2}`, keys accept any expression; inside map-literal context, a bare identifier before `:` is treated as atom key.
- Map access: `map[expr]`.
- Records: literals and access/update as shown above.
- Parentheses influence precedence.
- Function calls and operator calls.

### Operators and precedence

Infix and prefix operators are defined in `kernel/native_functions.v` and used by the Pratt parser. Examples (not exhaustive):
- Arithmetic: `+ - * /`
- Comparison: `== != < <= > >=`
- Logical: `and or`
- Bitwise: `&&& ||| ^^^ <<< >>>`
- Send: `pid ! msg`

Precedence follows the kernel table; parentheses can be used to override.

### Control flow

If expression:
```lx
if cond do
  expr_then
else
  expr_else
end
```
- Compiles to `case cond of true -> ...; false -> ... end`.

Case expression:
```lx
case expr do
  pattern [when guard] -> body
  ...
end
```
- Patterns support literals, variables, lists (`[]`, `[h|t]`), tuples (`{...}`), records (`Type{field: pat}`), and binaries (`<<...>>`).
- Bodies can contain multiple expressions separated by newline/semicolon.

With expression (monadic match):
```lx
with pattern <- expr, pattern2 <- expr2 do
  body
else
  other_pattern -> else_body
end
```
- Translates to nested `case` expressions. `else` can be a block or case-style clauses.

Match..rescue expression:
```lx
match pattern <- expr rescue error do
  rescue_body
end
```
- Compiles to `try ... of pattern -> ok catch Error -> rescue_body end`.

### Concurrency

- Spawn: `spawn(fn() -> body end)`
- Send: `pid ! message`
- Receive:
```lx
receive do
  pattern -> body
  pattern2 -> body2
end
```
- Supervisors/workers (syntax parsed; codegen emits functions with metadata-like comments):
```lx
supervisor name do
  ...
end

worker name do
  ...
end
```

### Binaries and bitstrings

Binary literal and pattern forms are supported uniformly:
```lx
<<1, 2, 3>>
<<"hello">>
<<version:8, size:16/big, payload:size/binary>>
```
- Segment grammar: `value[:size][/opt[-opt...]]`.
- Options include: `integer`, `float`, `binary`, `bitstring`, `utf8`, `utf16`, `utf32`, `signed`, `unsigned`, `big`, `little`, `native`, `unit:N`.
- Pattern binding uses `<<...>> = expr`.

### Module system metadata

- Dependencies: `deps [:cowboy, :jsx]`
- Import: `import :cowboy`
- Application config:
```lx
application {
  description: "My App",
  vsn: "1.0.0",
  applications: [:kernel, :stdlib],
  deps: [:cowboy]
}
```
- Codegen currently emits these as comments in Erlang output.

### Lambdas and anonymous functions

Single-line:
```lx
fn(x, y) -> x + y
```

Multiline with `end`:
```lx
fn(x) ->
  y = x * 2
  y + 1
end
```

Multi-head (pattern-based):
```lx
fn do
  (:ok) -> "success"
  (:error) -> "failure"
  (_) -> "unknown"
end
```

Lambda call (dot-call): `lambda.(args)`.

### Testing and directives

- Documentation: `@moduledoc "..."`, `@doc "..."` (affect generator: `-moduledoc`/`-doc`).
- Test blocks:
```lx
describe "Math" do
  test "adds" do
    assert sum(2,3) == 5
  end
end
```
- `assert` token is lexed; semantic behavior is not implemented in codegen (test blocks generate Erlang functions with a `test_` prefix).

## Error handling and diagnostics

The parser provides precise error messages for missing delimiters/keywords (e.g., expected `do`, `end`, closing `)`/`}`/`>>`). The analyzer enforces single-assignment and reports undefined variables (per tests). The generator emits `-spec` based on the type table when available.

## Examples

See `lx1/examples` and the test suite in `lx1/tests` for authoritative examples exercised by the compiler.