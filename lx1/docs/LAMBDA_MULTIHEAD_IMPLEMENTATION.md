# Lambda Multi-Head and Dot Call Implementation

## Overview
This document describes the implementation of two advanced lambda features in LX:
1. **Multi-head lambda functions** - Lambda functions with multiple pattern-matching clauses
2. **Dot call syntax** - Calling lambda functions using the `fun.(args)` syntax similar to Elixir

## Features Implemented

### 1. Lambda Functions with Type Annotations

**Syntax:**
```lx
# Single line lambda
lambda = fn(param :: type, param2 :: type) -> expression

# Multi-line lambda (requires 'end')
lambda = fn(param :: type) ->
    expression1
    expression2
end

# Block lambda
lambda = fn(param :: type) do
    expression1
    expression2
end
```

**Examples:**
```lx
# Single line
add = fn(x :: integer, y :: integer) -> x + y

# Multi-line (end required)
calc = fn(x :: integer, y :: integer) ->
    result = x + y
    result * 2
end

# Block syntax
process = fn(x :: integer) do
    doubled = x * 2
    doubled + 1
end
```

### 2. Multi-Head Lambda Functions

**Syntax:**
```lx
lambda = fn do
    (pattern1) -> expression1
    (pattern2) -> expression2
    (pattern3) -> expression3
end
```

**Example:**
```lx
pattern_matcher = fn do
    (:ok) -> "success"
    (:error) -> "failure"
    (_) -> "unknown"
end
```

**Generated Erlang:**
```erlang
PATTERN_MATCHER_1 = fun
    (ok) ->
        <<"success"/utf8>>;
    (error) ->
        <<"failure"/utf8>>;
    (__2) ->
        <<"unknown"/utf8>>
end
```

### 3. Lambda Call with Dot Syntax

**Syntax:**
```lx
lambda_function.(arg1, arg2, ...)
```

**Example:**
```lx
result = pattern_matcher.(:ok)
```

**Generated Erlang:**
```erlang
RESULT_3 = PATTERN_MATCHER_1(ok)
```

## Implementation Details

### AST Changes
- Added `lambda_call` to `NodeKind` enum in `ast/node.v`
- Added `new_lambda_call` builder function in `ast/builders.v`

### Parser Changes
- Modified `parse_lambda_expression` to support multi-head syntax with `fn do ... end`
- Modified `parse_record_access` to detect `.(` and call `parse_lambda_call`
- Added `parse_lambda_call` function to handle lambda call parsing

### Analyzer Changes
- Added `analyze_lambda_call` function for semantic analysis
- Integrated lambda call analysis into the main `analyze_node` match

### Generator Changes
- Modified `generate_lambda_expression` to distinguish between regular and multi-head lambdas
- Added `generate_function_clause` for generating lambda clauses without function names
- Added `generate_lambda_call` for generating lambda calls

### Key Implementation Points

1. **Type Annotation Support**: Lambda parameters now support full type annotations using `param :: type` syntax
2. **Multi-line Syntax**: If a lambda breaks line after `->`, it must be closed with `end`
3. **Multi-head Detection**: The parser checks if a lambda starts with `fn do` instead of `fn(`
4. **Clause Generation**: Multi-head lambdas generate Erlang `fun` expressions with multiple clauses separated by `;`
5. **Dot Call Parsing**: The parser extends record access logic to detect `.(` and parse as lambda call
6. **Type Safety**: Lambda parameters use proper type extraction from annotations, same as regular functions

## Testing

Complete test examples can be found in:
- `examples/task_11/advanced_features.lx` - Contains `test_multihead_lambdas` function
- Test suite in `tests/syntax_complete_test.v` includes comprehensive lambda tests

## Erlang Compatibility

The generated code is fully compatible with Erlang/OTP and follows standard Erlang lambda conventions:
- Multi-head functions use `fun` with pattern-matching clauses
- Lambda calls use direct function application syntax
- All patterns and guards are preserved in the translation