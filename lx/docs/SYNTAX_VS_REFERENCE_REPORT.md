# LX1 Syntax vs docs/pt_br/REFERENCIA_LINGUAGEM.md

This report compares the LX1 implemented syntax with the features described in `docs/pt_br/REFERENCIA_LINGUAGEM.md`.

## Legend
- ✅ Implemented and tested in lx1
- ⚠️ Parsed/Generated, but partially implemented or with limitations
- ❌ Not implemented in lx1

## Overview by topic

### Project creation and CLI
- The reference describes `lx new`, `lx compile`, `lx shell`, etc. This is out of scope for lx1 (compiler core in V). Not applicable here.

### Identifiers and comments
- ✅ Comments with `#` — supported by lexer.
- ✅ Identifiers and atoms — supported.

### Literals
- ✅ Integers, floats, strings with escapes, booleans, atoms, `nil` — supported and tested.
- ⚠️ String interpolation `${...}` in doc: not implemented as a surface syntax; generator has a node for interpolation but parser does not build it. Use binaries or concatenation instead.

### Variables and scope
- ✅ Bindings `x = expr` — supported; single-assignment enforced.
- ✅ Pattern bindings — lists, tuples, records, binaries — supported.
- ✅ Blocks: multi-expression blocks inside function bodies, case clauses, lambda bodies.

### Data types
- Lists — ✅ Literals and cons `[h | t]`; operators `++`, `length`, `in` are supported via kernel in lx5; in lx1 docs, list concatenation operator is available via kernel table only if defined. The tests cover list basics from tasks 5; lx1 implements lists per parser/generator.
- Tuples — ✅ Literals and patterns.
- Maps — ✅ Literals `%{}`; access `map[key]`; updates in record-compat syntax `%Type{expr | field: value}`; plain map update with `%{map | k: v}` semantics is processed as record-update in lx1 and generates record update. For maps, use literals and `maps:get/2` access; updates as map-update sugar are not yet distinct from records.
- Records — ✅ Definitions, literals, access, update; codegen emits Erlang `-record` and `#rec{}`.
- Binaries — ✅ Full literal and pattern segment syntax, sizes and options, including chained options and `unit:N`.

### Functions
- ✅ `def name(args) do ... end` (single-body)
- ✅ `def name do (pattern) [when guard] -> body ... end` (multi-head)
- ✅ Anonymous functions `fn(...) -> ... end`, multi-head `fn do ... end`, and dot-call `lambda.(...)`.
- ✅ Type annotations on parameters `arg :: type` (used for analysis/specs; generator uses type table).

### Control flow
- ✅ `if ... do ... [else ...] end` — implemented via case.
- ✅ `case expr do pattern [when guard] -> body ... end` — implemented with full patterns.
- ✅ `with pattern <- expr [, ...] do ... [else clauses|block] end` — implemented with nested case generation.
- ✅ `match pattern <- expr rescue err do ... end` — implemented as try/catch in generator.

### Guards
- ✅ `when` expressions inside `case` clauses and function heads.

### Concurrency
- ✅ `spawn(fn() -> ... end)`
- ✅ Send operator `pid ! msg`
- ✅ `receive do pattern -> body ... end`
- ⚠️ `after timeout -> ...` clause is shown in reference; not implemented in lx1 `receive` parser/generator.
- ⚠️ Supervisors/workers syntax parsed; generator emits stub functions/comments, not OTP behaviours.

### Custom types
- ✅ `type name :: Type` (simple aliases)
- ✅ `type opaque name :: Type`
- ✅ `type nominal name :: Type`
- ✅ Parameterized type headers `type name(T, U, ...) :: Body`
- ⚠️ Union types `A | B` shown in reference — parser-side union node is present in builders but top-level `type ... :: A | B` union parsing is not wired; current `parse_type_expression` parses identifiers with type arguments only. Tests cover simple aliases and opaque/nominal. Union/generic body syntax is a TODO.

### Module system
- ✅ `deps [:pkg, :pkg2]`
- ✅ `import :module`
- ✅ `application { ... }`
- ⚠️ These are emitted as comments in Erlang; no build-time integration.

### Errors/with/match from reference
- ✅ With/match covered above. Try/catch is covered by `match ... rescue` generation.

### Directives and tests
- ✅ `@moduledoc`, `@doc` — affect generator output.
- ✅ `describe` and `test` blocks parsed; generator emits test_ functions. Assertions are not executed/translated.

### Operators differences
- ✅ Not-equal operator is `!=` (as required by project convention).
- ✅ Logical `and/or` map to Erlang `andalso/orelse` via kernel.

## Gaps and differences vs reference

- ❌ `receive ... after timeout -> ... end` — not implemented.
- ❌ Full module system (deps resolution, beam validation) — not implemented; only metadata emitted.
- ❌ List comprehensions `[expr || gen, filter]` — not parsed; generator has a node but parser doesn’t build it.
- ❌ String interpolation `"Hello, #{name}"` — not parsed; can be composed with binaries.
- ⚠️ Map update sugar `%{map | k: v}` in reference: lx1 `%{ ... }` after `%` is reserved for record update; there is no general map update lowering yet.
- ⚠️ Union types and complex type bodies in `type ... :: ...` — limited parsing; current parser accepts identifier types and nested type applications, but not `A | B` textual unions.
- ⚠️ Guards: reference shows guards in various places; lx1 supports them in case/function-heads; no separate guard section syntax.
- ⚠️ Test framework semantics (`assert`, runner) — parsed only; no semantics.

## Conclusion

LX1 implements the core of the reference language: literals, variables, lists/tuples/maps/records, operators with precedence, control flow (if/case/with), binaries, lambdas (including multi-head), pattern matching, and a metadata-only module system. Missing items are mainly ergonomics (string interpolation, list comprehensions), richer types (textual unions), `receive ... after`, and functional module integration.