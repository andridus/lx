# LX Parser1 - Grammar-Compliant Parser

This is a complete refactoring of the LX parser that strictly follows the grammar specification defined in `docs/# LX Grammar and Structural Model.md`.

## Architecture Overview

### Single Entry Point Design
- **`parser.v`** - The ONLY public entry point with `parse_program()` function
- All parsing flows through this single function, ensuring consistency

### Strict Context Management
The parser enforces strict parsing contexts as defined in the grammar:
- **`mod` context** - For program and block_top_level (only module_statements allowed)
- **`expression` context** - For block_expression and function bodies (expressions allowed)

### File Organization

#### Core Infrastructure
- **`internal/core_parser.v`** - Core parser infrastructure with context management
  - ParsingContext enum (mod, expression)
  - LXParser struct with context tracking
  - Token navigation and error handling
  - Context switching utilities

#### Module-Level Parsing (mod context)
- **`internal/module_statements.v`** - Top-level module statement parsing
  - `parse_program_statements()` - Main program entry point
  - Function definitions (def, defp)
  - Record definitions
  - Type definitions
  - Spec definitions
  - Test definitions
  - Worker/Supervisor definitions
  - Describe blocks

#### Expression-Level Parsing (expression context)
- **`internal/block_expressions.v`** - Expression parsing in expression context
  - `parse_block_expression()` - Block expressions (do...end)
  - `parse_expression()` - All expression types
  - Assignment expressions
  - Logical expressions (or, and)
  - Equality expressions (==, !=)
  - Comparison expressions (<, >, <=, >=)
  - Arithmetic expressions (+, -, *, /)
  - Unary expressions (-, not)
  - Primary expressions (literals, identifiers, etc.)
  - Complex expressions (if, case, tuples, lists)

#### Type System Parsing
- **`internal/type_expressions.v`** - Type expression parsing (separate from value expressions)
  - `parse_type_expression()` - Main type parsing entry point
  - Union types (type1 | type2)
  - Named types and parameterized types
  - Record types (RecordName{field: type})
  - Function types ((param_types) -> return_type)
  - Tuple types ({type1, type2})

#### Pattern Matching
- **`internal/pattern_expressions.v`** - Pattern parsing for pattern matching
  - `parse_pattern()` - Main pattern parsing entry point
  - Variable patterns with optional type annotations
  - Literal patterns (integers, strings, atoms, etc.)
  - Complex patterns (tuples, lists, records, maps)
  - Cons patterns ([head | tail])

#### Function Definitions
- **`internal/function_clauses.v`** - Function clause parsing with proper context management
  - `parse_function_clauses()` - Main function clause entry point
  - Single-clause functions: `def func(...) do ... end`
  - Multi-clause functions: `def func do (...) -> ... (...) -> ... end`
  - Function headers with parameters, guards, and bodies
  - Proper context switching between mod and expression contexts

## Grammar Compliance

### Top-Level Structure (mod context)
```ebnf
program ::= { module_statement }

module_statement ::=
    function_definition
  | record_definition
  | type_definition
  | spec_definition
  | test_definition
  | worker_definition
  | supervisor_definition
  | describe_block
```

**Enforcement**: The parser REJECTS any expressions at the module level. Only structural declarations are allowed.

### Block Types

#### block_top_level (mod context)
```ebnf
block_top_level ::= 'do' { module_statement } 'end'
```
Used in: worker, supervisor, describe blocks
**Enforcement**: Maintains mod context, only allows module_statements

#### block_expression (expression context)
```ebnf
block_expression ::= 'do' expression_list 'end'
expression_list ::= expression { expression }
```
Used in: function bodies, if/case expressions
**Enforcement**: Switches to expression context, allows all expressions

### Context Switching
The parser uses the `with_context()` function to safely switch between contexts:
```v
// Switch to expression context for function body
body := p.with_context(.expression, fn (mut parser LXParser) ?ast.Expr {
    return parser.parse_block_expression()
})?
```

## Key Features

### 1. Strict Grammar Enforcement
- No expressions allowed at module level
- Clear separation between structural and expression contexts
- Proper context validation at each parsing step

### 2. Clear Separation of Concerns
- Types parsing separated from expression parsing
- Patterns parsing separated from expression parsing
- Function clauses with proper context management
- Module statements clearly defined

### 3. Comprehensive Error Handling
- Context-aware error messages
- Clear indication of what's expected vs what was found
- Proper error recovery and synchronization

### 4. Maintainable Architecture
- Single entry point eliminates confusion
- Clear file organization by parsing concern
- Well-documented functions with grammar references
- Consistent naming conventions

## Usage

```v
import parser

// Create parser with tokens from lexer
mut p := parser.new_parser(tokens)

// Parse entire program (single entry point)
module_ast := p.parse_program() or {
    // Handle parsing errors
    for error in p.get_errors() {
        println('Parse error: ${error}')
    }
    return
}
```

## Testing Strategy

Each parsing context and major feature should have dedicated tests:
- Module statement parsing tests
- Expression parsing tests
- Type expression parsing tests
- Pattern parsing tests
- Function clause parsing tests
- Context enforcement tests
- Error handling tests

This ensures the parser correctly implements the grammar specification and maintains strict context separation.