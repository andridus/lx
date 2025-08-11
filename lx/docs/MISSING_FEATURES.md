# LX1 Missing Features and Next Steps

This document enumerates the notable syntax and semantic features that are not fully implemented in lx1, based on the current lexer/parser/generator and the test suite.

## High-priority gaps

1. Receive after clause
   - Syntax: `receive do ... after N -> expr end`
   - Parser: extend `parse_receive_expression` to accept optional `after integer -> expr` (and also allow trailing newline/semicolon handling).
   - Generator: emit Erlang `after` block inside `receive`.

2. String interpolation
   - Syntax: `"Hello, #{name}!"` as in the reference.
   - Parser: introduce a dedicated interpolation lexer mode or parse interpolation within `read_string` producing `string_interpolation` nodes with segment list (strings and expressions).
   - Generator: already has `generate_string_interpolation`; wire parser to produce these nodes.

3. List comprehensions
   - Syntax: `[expr || gen, filter, ...]`.
   - Parser: add comprehension production building `list_comprehension` node with expression, generators, and filters.
   - Generator: `generate_list_comprehension` already exists; use it.

4. Union types in type definitions
   - Syntax: `type result(T) :: {:some, T} | :none` and plain `A | B` unions.
   - Parser: extend `parse_type_expression` to parse `type_or` grammar with `|` at type level; introduce `union_type` nodes in type AST (builders exist).
   - Analyzer: ensure the type table can store/emit unions; generator maps unions in `type_to_erlang_spec` (already supported when Type.name == 'union').

5. Map update sugar distinct from record update
   - Syntax: `%{map | key: value}` for maps vs `%Type{rec | field: value}` for records.
   - Parser: current `%{...}` path interprets as record update; add disambiguation and codegen to emit `maps:put/3` or map update patterns (or keep as a sequence of puts for multiple fields).

6. Module system integration
   - Deps/import/application are currently comments in Erlang output.
   - Implement: CLI/build integration (outside lx1 scope) and optional static checks (e.g., validate imports, ensure deps exist).

## Medium-priority gaps

7. `try/catch` expression (explicit)
   - Reference says not necessary now, but `match ... rescue` is present; explicit try/catch could be added later for completeness.

8. `receive` timeout guards and variable scope validations
   - Add checks for variable binding inside receive clauses and optional timeouts.

9. Test framework semantics
   - Implement `assert` lowering and a minimal runtime to execute `describe/test` blocks, or emit EUnit/CT compatible code.

10. Guards expansion and builtins
   - Ensure standard guard BIFs are recognized (`is_integer/1`, etc.) and mapped appropriately in generator.

11. Better error messages for case/with pattern blocks
   - The parser already tries to detect patterns at line start; keep improving heuristics and suggestions.

## Low-priority / ergonomics

12. Numeric literal niceties
   - Underscore separators in numbers; more bases; better float formats. Lexer already handles 0x/0o/0b and BaseB.

13. Doc/spec directives expansion
   - Support `@spec` directive and emit corresponding `-spec` if not already provided by analyzer.

14. Supervisor/worker DSL
   - Decide final surface syntax and semantics (OTP integration), or move to a separate layer.

## Done (for visibility)

- Control flow: `if`, `case`, `with`, `match..rescue` — implemented and tested.
- Binaries: literals, patterns, sizes/options — implemented and tested.
- Records, tuples, lists, maps (literals/access), operators with precedence — implemented and tested.
- Lambdas (single/multi-head) and dot-call — implemented and tested.
- Types: simple, opaque, nominal; parameter headers — implemented; unions pending.
- Module metadata: `deps`, `import`, `application` — emitted as comments.

## Suggested roadmap

1) Receive `after` and string interpolation (parser/generator) — fast wins leveraging existing generator nodes.
2) List comprehensions (parser) — generator is ready.
3) Type unions parsing — extend type expression grammar; analyzer/type_table support.
4) Map update sugar — parser disambiguation and codegen to `maps:put/3`.
5) Optional: test runtime/assert lowering; module integration as a separate project task.