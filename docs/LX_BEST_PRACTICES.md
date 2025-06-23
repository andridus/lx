## Lx Best Practices Guide

This guide provides actionable best practices for writing clear, maintainable, and idiomatic Lx code. It emphasizes clean abstractions, Erlang/OTP conventions, and process-oriented design.

---

### 1. Function Visibility and Structure

- Use `pub` only for functions that are part of the module's external API.
- Keep private helper functions unmarked.
- Organize functions by lifecycle: `init`, `handle_call`, `handle_cast`, then helpers.
- Avoid overly long modules; prefer focused logical boundaries.

---

### 2. Naming Conventions

- Use `snake_case` consistently for all identifiers.
- Avoid cryptic abbreviations or overly clever names.
- Use `__MODULE__` for referring to the current module (for logs, messages, etc.).
- Be explicit in your names, even if longer reassignment is forbidden.
- For stateful modules (like workers), always return the updated state explicitly.
- Prefer tuple state (e.g., `.{{count, 0}, timer}`) for clarity.

---

### 7. OTP Component Design

- `worker` is for modules that hold and transform state.
- `supervisor` defines restart strategy and children.
- Use `children [...]` directly for short lists, or `children {}` for more complex definitions.

```lx
supervisor my_sup {
 strategy one_for_one
 children [my_worker]
}
```

---

### 8. Message Passing

- Always validate the shape of messages received.
- Prefer tagged tuples (`.{:event, data}`) over loose atoms or values.
- Include a fallback pattern in every `receive`.

```lx
receive {
 .{:log, Msg} -> log(Msg)
 _ -> :ignore
}
```

---

### 9. Logging and Side Effects

- Isolate side-effectful code like logging, HTTP, or DB.
- Use `_ = log(...)` when return value is irrelevant.
- Prefer tracing through tagged tuples and standard metadata.

---

### 10. Testing Philosophy

- Group tests by behavior using `describe`.
- Use `test` blocks with clear assertions:

```lx
describe "utils" {
 test "capitalizes a word" {
   assert capitalize("lx") == "Lx"
 }
}
```

- Avoid testing private functions directly aligned with decades of Erlang/OTP excellence.

---
