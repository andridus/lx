# Variable Scope Implementation Guide

This document outlines the implementation strategy for variable scope checking in the LX language compiler, ensuring proper variable usage, preventing rebinding, and managing lexical scoping according to LX language specifications.

---

## Executive Summary

### Problem Statement
Currently, the LX compiler does not validate variable usage, allowing undefined variables to be used and missing critical scope-related errors. This leads to runtime errors and poor developer experience.

### Solution Overview
Implement a comprehensive variable scope checking system that:
- **Validates variable definitions** before usage
- **Prevents rebinding** within the same scope
- **Blocks shadowing** from outer scopes
- **Allows parallel variables** in separate blocks
- **Provides clear error messages** with helpful suggestions

### Key Implementation Points
1. **Extend existing typechecker** infrastructure for variable scoping
2. **Use hierarchical scope management** with parent-child relationships
3. **Integrate with AST traversal** to check expressions and statements
4. **Follow LX error reporting patterns** with didactic suggestions
5. **Maintain performance** with efficient scope lookups

### Expected Impact
- **Compile-time error detection** for undefined variables
- **Improved code quality** through strict scoping rules
- **Better developer experience** with clear error messages
- **Consistency** with LX language design principles

---

## Overview

LX enforces strict variable scoping rules to maintain code clarity and prevent common programming errors. The implementation will add compile-time checks to ensure:

1. **Variables are defined before use**
2. **No rebinding within the same scope**
3. **No shadowing from outer scopes**
4. **Proper scope isolation for parallel blocks**

---

## Variable Scoping Rules

### Core Principles

1. **Immutability**: Variables cannot be reassigned once defined
2. **No Shadowing**: Variables from outer scopes cannot be redefined in inner scopes
3. **Parallel Scope Isolation**: Variables can be reused in parallel blocks if not defined in common parent scope
4. **Lexical Scoping**: Variable visibility follows the lexical structure of the code

### Valid Patterns

```lx
# ✅ Parallel variables in separate blocks
def example1() do
  block1 = do
    temp = 10
    temp * 2
  end
  block2 = do
    temp = 20  # OK: temp not defined in function scope
    temp + 5
  end
  block1 + block2
end

# ✅ Nested blocks with different variables
def example2() do
  outer = 100
  result = do
    inner = 200
    outer + inner  # OK: outer from parent scope
  end
  result
end
```

### Invalid Patterns

```lx
# ❌ Rebind in same scope
def invalid1() do
  x = 10
  x = 20  # Error: rebind not allowed
  x
end

# ❌ Shadowing from outer scope
def invalid2() do
  shared = 100
  block = do
    shared = 200  # Error: shadows variable from outer scope
    shared
  end
  block
end

# ❌ Undefined variable
def invalid3() do
  result = undefined_var  # Error: variable not defined
  result
end
```

---

## Implementation Strategy

### 1. Scope Management Structure

Extend the existing typechecker infrastructure to handle variable scoping:

```v
// VariableScope represents a lexical scope for variable checking
pub struct VariableScope {
pub mut:
    variables map[string]VariableBinding
    parent    ?&VariableScope
    level     int
}

// VariableBinding represents a variable binding
pub struct VariableBinding {
pub:
    name     string
    position Position
    defined  bool
}

// VariableChecker provides variable scope checking functionality
pub struct VariableChecker {
pub mut:
    current_scope &VariableScope
    errors        []CompilationError
}
```

### 2. Core Methods

#### Scope Management

```v
// enter_scope enters a new nested scope
pub fn (mut vc VariableChecker) enter_scope() {
    new_scope := &VariableScope{
        variables: map[string]VariableBinding{}
        parent:    vc.current_scope
        level:     vc.current_scope.level + 1
    }
    vc.current_scope = new_scope
}

// exit_scope exits the current scope and returns to parent
pub fn (mut vc VariableChecker) exit_scope() {
    if parent := vc.current_scope.parent {
        vc.current_scope = parent
    }
}

// bind_variable adds a variable to the current scope
pub fn (mut vc VariableChecker) bind_variable(name string, position Position) {
    vc.current_scope.variables[name] = VariableBinding{
        name:     name
        position: position
        defined:  true
    }
}
```

#### Variable Lookup

```v
// has_binding_local checks if variable exists in current scope only
pub fn (vc &VariableChecker) has_binding_local(name string) bool {
    return name in vc.current_scope.variables
}

// has_binding_recursive checks if variable exists in current or parent scopes
pub fn (vc &VariableChecker) has_binding_recursive(name string) bool {
    mut current := vc.current_scope
    for current != unsafe { nil } {
        if name in current.variables {
            return true
        }
        current = current.parent or { unsafe { nil } }
    }
    return false
}

// has_binding_in_parent checks if variable exists in parent scopes only
pub fn (vc &VariableChecker) has_binding_in_parent(name string) bool {
    if parent := vc.current_scope.parent {
        mut current := parent
        for current != unsafe { nil } {
            if name in current.variables {
                return true
            }
            current = current.parent or { unsafe { nil } }
        }
    }
    return false
}
```

### 3. Expression Checking

#### Assignment Expression

```v
// check_assignment_expression validates assignment expressions
pub fn (mut vc VariableChecker) check_assignment_expression(expr ast.AssignExpr) {
    // Check for rebind in current scope
    if vc.has_binding_local(expr.name) {
        vc.report_error(
            "Variable '${expr.name}' cannot be reassigned",
            "Variables in LX are immutable and cannot be reassigned",
            expr.position
        )
        return
    }

    // Check for shadowing from parent scopes
    if vc.has_binding_in_parent(expr.name) {
        vc.report_error(
            "Variable '${expr.name}' shadows variable from outer scope",
            "Shadowing is not allowed in LX. Use a different variable name",
            expr.position
        )
        return
    }

    // Check the value expression
    vc.check_expression(expr.value)

    // Bind the variable to current scope
    vc.bind_variable(expr.name, expr.position)
}
```

#### Variable Expression

```v
// check_variable_expression validates variable usage
pub fn (mut vc VariableChecker) check_variable_expression(expr ast.VariableExpr) {
    if !vc.has_binding_recursive(expr.name) {
        vc.report_error(
            "Variable '${expr.name}' is not defined",
            "Variables must be defined before use. Check spelling or add an assignment",
            expr.position
        )
    }
}
```

### 4. Statement and Block Checking

#### Function Statement

```v
// check_function_statement validates function definitions
pub fn (mut vc VariableChecker) check_function_statement(stmt ast.FunctionStmt) {
    vc.enter_scope()

    // Add function parameters to scope
    for clause in stmt.clauses {
        for param in clause.parameters {
            vc.check_pattern(param)
        }

        // Check function body
        for body_stmt in clause.body {
            vc.check_statement(body_stmt)
        }
    }

    vc.exit_scope()
}
```

#### Block Expression

```v
// check_block_expression validates do...end blocks
pub fn (mut vc VariableChecker) check_block_expression(expr ast.BlockExpr) {
    vc.enter_scope()

    for stmt in expr.statements {
        vc.check_statement(stmt)
    }

    vc.exit_scope()
}
```

### 5. Pattern Matching

#### Variable Pattern

```v
// check_variable_pattern validates variable patterns
pub fn (mut vc VariableChecker) check_variable_pattern(pattern ast.VarPattern) {
    // In patterns, variables are bound (not used)
    vc.bind_variable(pattern.name, pattern.position)
}
```

---

## Integration with Existing Compiler

### 1. Integration Point

Add variable checking to the existing typechecker or create a separate analysis pass:

```v
// Add to existing typechecker
pub fn (mut tc TypeChecker) check_module(module ast.ModuleStmt) TypeCheckResult {
    mut var_checker := new_variable_checker()

    // Check all statements
    for stmt in module.statements {
        var_checker.check_statement(stmt)
    }

    // Combine type errors with variable errors
    mut all_errors := tc.errors.clone()
    all_errors << var_checker.errors

    return TypeCheckResult{
        success: all_errors.len == 0
        errors:  all_errors
    }
}
```

### 2. Error Reporting

Use the existing error reporting system:

```v
// report_error adds a variable-related error
pub fn (mut vc VariableChecker) report_error(message string, suggestion string, position Position) {
    error := CompilationError{
        category: .variable_error
        message:  message
        suggestion: suggestion
        position: position
        severity: .error
    }
    vc.errors << error
}
```

---

## Test Cases

### Valid Cases

```v
fn test_parallel_variables() {
    source := '
def test() do
  block1 = do
    x = 10
    x * 2
  end
  block2 = do
    x = 20
    x + 5
  end
  block1 + block2
end'

    result := check_variables(source)
    assert result.success == true
    assert result.errors.len == 0
}

fn test_nested_scopes() {
    source := '
def test() do
  outer = 100
  result = do
    inner = 200
    outer + inner
  end
  result
end'

    result := check_variables(source)
    assert result.success == true
    assert result.errors.len == 0
}
```

### Invalid Cases

```v
fn test_rebind_error() {
    source := '
def test() do
  x = 10
  x = 20
  x
end'

    result := check_variables(source)
    assert result.success == false
    assert result.errors.len == 1
    assert result.errors[0].message.contains("cannot be reassigned")
}

fn test_shadowing_error() {
    source := '
def test() do
  shared = 100
  block = do
    shared = 200
    shared
  end
  block
end'

    result := check_variables(source)
    assert result.success == false
    assert result.errors.len == 1
    assert result.errors[0].message.contains("shadows variable")
}

fn test_undefined_variable() {
    source := '
def test() do
  result = undefined_var
  result
end'

    result := check_variables(source)
    assert result.success == false
    assert result.errors.len == 1
    assert result.errors[0].message.contains("is not defined")
}
```

---

## Error Messages

### Standard Error Messages

1. **Undefined Variable**:
   ```
   Variable 'undefined_var' is not defined

   💡 Suggestion:
      Variables must be defined before use. Check spelling or add an assignment:
         undefined_var = some_value
   ```

2. **Rebind Error**:
   ```
   Variable 'x' cannot be reassigned

   💡 Suggestion:
      Variables in LX are immutable and cannot be reassigned.
      Use a different variable name or restructure your code.
   ```

3. **Shadowing Error**:
   ```
   Variable 'shared' shadows variable from outer scope

   💡 Suggestion:
      Shadowing is not allowed in LX. Use a different variable name:
         inner_shared = 200
   ```

---

## Performance Considerations

### Memory Management

- Use reference counting for scope objects
- Implement scope cleanup on exit
- Consider scope pooling for frequently created/destroyed scopes

### Optimization

- Cache variable lookups for frequently accessed variables
- Use hash maps for O(1) variable lookup
- Minimize scope traversal for deep nesting

---

## Future Enhancements

### Potential Improvements

1. **Variable Usage Analysis**: Track unused variables
2. **Dead Code Detection**: Identify unreachable code
3. **Variable Hoisting Warnings**: Warn about potential hoisting issues
4. **Scope Visualization**: Debug tools for scope inspection

### Integration with IDE

- Real-time variable checking in editor
- Variable highlighting and navigation
- Auto-completion with scope awareness
- Refactoring support with scope validation

---

## Conclusion

This implementation provides comprehensive variable scope checking for the LX language, ensuring code quality and preventing common programming errors. The design integrates seamlessly with the existing compiler infrastructure while maintaining the strict scoping rules that make LX a reliable and predictable language.

The implementation follows LX's design principles of clarity, safety, and developer experience, providing helpful error messages and suggestions to guide developers toward correct code patterns.