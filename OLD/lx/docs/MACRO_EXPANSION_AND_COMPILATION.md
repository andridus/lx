# LX Language - Macro Expansion and Compilation Implementation

## Overview

This document outlines the implementation plan for the next phase of LX development: **macro expansion** (using defined macros) and **compilation to Erlang** (generating .erl and .beam files).

## Current Status

### ✅ **Implemented:**
- Macro definition parsing (`defmacro`)
- AST generation for macro definitions
- Basic lexer and parser infrastructure
- Global installation and CLI

### 🔄 **Next Phase Goals:**
1. **Macro Call Recognition** - Parse macro invocations
2. **Macro Expansion** - Transform macro calls to expanded AST
3. **Erlang Code Generation** - Convert AST to .erl files
4. **Beam Compilation** - Compile .erl to .beam files

## Phase 2A: Macro Call Recognition

### 1. Extend Lexer (`lx_lexer.xrl`)

**Add support for macro call syntax:**
```erlang
% Macro call tokens
macro_call : {token, {macro_call, TokenLine}}.

% Support for macro names as atoms
macro_name : {token, {macro_name, TokenLine, list_to_atom(TokenChars)}}.
```

### 2. Extend Parser (`lx_parser.yrl`)

**Add grammar rules for macro calls:**
```erlang
% Macro call expressions
expression -> macro_call : '$1'.

% Different macro call patterns
macro_call -> atom '(' expression_list ')' :
    {macro_call, line('$1'), element(3, '$1'), '$3'}.

macro_call -> atom do_block :
    {macro_call_block, line('$1'), element(3, '$1'), '$2'}.

macro_call -> atom expression :
    {macro_call_simple, line('$1'), element(3, '$1'), '$2'}.
```

### 3. Example Macro Call Syntax

```lx
# Define macro
defmacro a(body) do
  {:a, {1, 1, any, [{:args, []}]}, [body]}
end

# Call macro (new syntax to support)
a [1,2,3]                    # Simple call
a do [1,2,3] end            # Block call
a(x, y)                     # Function-like call
```

## Phase 2B: Macro Expansion Engine

### 1. Enhance Macro Registry (`lx_macros.erl`)

**Extend the macro system to support expansion:**

```erlang
% Add expansion functions
expand_macro_call(Name, Arguments, Registry) ->
    case get_macro(Name, Registry) of
        {Metadata, Body} ->
            expand_macro_body(Body, Arguments, Metadata);
        not_found ->
            {error, {undefined_macro, Name}}
    end.

expand_macro_body(Body, Arguments, {Arity, _, _, Modifiers}) ->
    % Validate argument count
    case length(Arguments) of
        Arity ->
            substitute_arguments(Body, Arguments);
        _ ->
            {error, {wrong_arity, Arity, length(Arguments)}}
    end.
```

### 2. Argument Substitution

**Implement parameter substitution in macro bodies:**

```erlang
substitute_arguments(Body, Arguments) ->
    % Replace parameter references with actual arguments
    % This is a simplified version - full implementation needs
    % proper AST traversal and substitution
    substitute_in_ast(Body, Arguments).

substitute_in_ast({atom, Line, body}, [Arg | _]) ->
    % Replace 'body' parameter with first argument
    Arg;
substitute_in_ast(Node, _) when is_tuple(Node) ->
    % Recursively substitute in tuple elements
    list_to_tuple([substitute_in_ast(Elem, Arguments) || Elem <- tuple_to_list(Node)]);
substitute_in_ast(Node, _) ->
    % Return unchanged for other nodes
    Node.
```

### 3. Expansion Pipeline

**Update the compiler to include expansion:**

```erlang
% In lx_compiler.erl
compile(Source) ->
    case lx_lexer:string(Source) of
        {ok, Tokens, _} ->
            case lx_parser:parse(Tokens) of
                {ok, AST} ->
                    % First pass: register all macros
                    Registry = register_all_macros(AST),
                    % Second pass: expand macro calls
                    ExpandedAST = expand_all_macros(AST, Registry),
                    {ok, ExpandedAST};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

## Phase 2C: Erlang Code Generation

### 1. Create Erlang Generator (`lx_erlang_generator.erl`)

**New module for generating Erlang code:**

```erlang
-module(lx_erlang_generator).
-export([generate/1, generate_file/2]).

% Generate Erlang code from LX AST
generate(AST) ->
    generate_module(AST).

generate_module(AST) ->
    % Extract module name and functions
    ModuleName = extract_module_name(AST),
    Functions = extract_functions(AST),

    % Generate Erlang module
    generate_erlang_module(ModuleName, Functions).

generate_erlang_module(ModuleName, Functions) ->
    % Generate Erlang syntax
    ModuleHeader = generate_module_header(ModuleName),
    FunctionCode = [generate_function(F) || F <- Functions],

    % Combine into complete module
    [ModuleHeader, "\n", FunctionCode].
```

### 2. AST to Erlang Translation

**Implement translation rules:**

```erlang
% Translate LX AST nodes to Erlang code
generate_function({macro_def, Line, Name, Params, Body}) ->
    % Convert macro definition to Erlang function
    ParamList = generate_params(Params),
    BodyCode = generate_expression(Body),

    io_lib:format("~s(~s) ->\n    ~s.\n",
                  [Name, ParamList, BodyCode]).

generate_expression({tuple, Line, Elements}) ->
    ElementCode = [generate_expression(E) || E <- Elements],
    "{" ++ string:join(ElementCode, ", ") ++ "}".

generate_expression({list, Line, Elements}) ->
    ElementCode = [generate_expression(E) || E <- Elements],
    "[" ++ string:join(ElementCode, ", ") ++ "]".

generate_expression({atom, Line, Value}) ->
    atom_to_list(Value).

generate_expression({integer, Line, Value}) ->
    integer_to_list(Value).
```

### 3. File Generation

**Generate .erl files:**

```erlang
generate_file(AST, Filename) ->
    ErlangCode = generate(AST),
    ErlFilename = filename:rootname(Filename, ".lx") ++ ".erl",

    case file:write_file(ErlFilename, ErlangCode) of
        ok -> {ok, ErlFilename};
        {error, Reason} -> {error, Reason}
    end.
```

## Phase 2D: Beam Compilation

### 1. Integrate with Erlang Compiler

**Add beam compilation to the pipeline:**

```erlang
% In lx_compiler.erl
compile_to_beam(Source, Options) ->
    case compile(Source) of
        {ok, AST} ->
            % Generate Erlang code
            case lx_erlang_generator:generate(AST) of
                {ok, ErlCode} ->
                    % Compile to beam
                    compile_erlang_to_beam(ErlCode, Options);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_erlang_to_beam(ErlCode, Options) ->
    % Use Erlang's compile module
    case compile:forms(ErlCode, Options) of
        {ok, ModuleName, BeamCode} ->
            % Write beam file
            BeamFilename = atom_to_list(ModuleName) ++ ".beam",
            case file:write_file(BeamFilename, BeamCode) of
                ok -> {ok, BeamFilename};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

### 2. CLI Integration

**Extend CLI to support compilation targets:**

```erlang
% In lx_cli.erl
main(["compile", Filename | Options]) ->
    case parse_compile_options(Options) of
        {ok, CompileOptions} ->
            case lx_compiler:compile_to_beam(Filename, CompileOptions) of
                {ok, BeamFile} ->
                    io:format("Compiled successfully: ~s~n", [BeamFile]),
                    halt(0);
                {error, Reason} ->
                    io:format("Compilation failed: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Invalid options: ~p~n", [Reason]),
            halt(1)
    end;

main(["generate", Filename]) ->
    case lx_compiler:compile_file(Filename) of
        {ok, AST} ->
            case lx_erlang_generator:generate_file(AST, Filename) of
                {ok, ErlFile} ->
                    io:format("Generated: ~s~n", [ErlFile]),
                    halt(0);
                {error, Reason} ->
                    io:format("Generation failed: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Compilation failed: ~p~n", [Reason]),
            halt(1)
    end.
```

## Implementation Plan

### Week 1: Macro Call Recognition
- [ ] Extend lexer for macro call tokens
- [ ] Add parser rules for macro call syntax
- [ ] Create test cases for macro calls
- [ ] Update AST structure for macro calls

### Week 2: Macro Expansion Engine
- [ ] Implement macro registry lookup
- [ ] Create argument substitution logic
- [ ] Build expansion pipeline
- [ ] Add error handling for expansion

### Week 3: Erlang Code Generation
- [ ] Create Erlang generator module
- [ ] Implement AST to Erlang translation
- [ ] Add file generation capabilities
- [ ] Create comprehensive test suite

### Week 4: Beam Compilation
- [ ] Integrate with Erlang compiler
- [ ] Add CLI support for compilation
- [ ] Implement error handling
- [ ] Create end-to-end tests

## Example Workflow

### Input LX File (`example.lx`):
```lx
defmacro double(x) do
  x * 2
end

defmacro if(condition, true_body, false_body) do
  case condition of
    true -> true_body;
    false -> false_body
  end
end

double(5)
if(true, "yes", "no")
```

### Generated Erlang File (`example.erl`):
```erlang
-module(example).
-export([double/1, if/3]).

double(X) ->
    X * 2.

if(Condition, TrueBody, FalseBody) ->
    case Condition of
        true -> TrueBody;
        false -> FalseBody
    end.

% Generated code from macro calls
main() ->
    double(5),
    if(true, "yes", "no").
```

### Compiled Beam File:
- `example.beam` - Executable Erlang bytecode

## Testing Strategy

### Unit Tests
- Macro call parsing
- Argument substitution
- AST to Erlang translation
- Beam compilation

### Integration Tests
- End-to-end compilation pipeline
- Error handling scenarios
- Performance benchmarks

### Example Tests
```erlang
% Test macro call parsing
test_macro_call_parsing() ->
    Source = "a [1,2,3]",
    {ok, AST} = lx_compiler:compile(Source),
    ?assertMatch([{macro_call, _, a, _}], AST).

% Test macro expansion
test_macro_expansion() ->
    Source = "defmacro a(x) do x end\na(5)",
    {ok, AST} = lx_compiler:compile(Source),
    ?assertMatch([{macro_def, _, a, _, _}, {integer, _, 5}], AST).
```

## CLI Commands

### New Commands to Add:
```bash
# Generate Erlang code only
lx-compiler generate example.lx

# Compile to beam file
lx-compiler compile example.lx

# Compile with options
lx-compiler compile example.lx --optimize --debug

# Show generated Erlang code
lx-compiler generate example.lx --show
```

## Error Handling

### Macro Expansion Errors:
- Undefined macro calls
- Wrong argument count
- Invalid argument types
- Circular macro dependencies

### Compilation Errors:
- Invalid Erlang syntax
- Compilation warnings
- File system errors
- Memory/performance issues

## Performance Considerations

### Optimization Strategies:
- Macro caching for repeated calls
- Lazy expansion for large ASTs
- Incremental compilation
- Parallel processing for multiple files

### Memory Management:
- AST size optimization
- Garbage collection tuning
- Memory leak prevention

## Future Enhancements

### Advanced Features:
- Macro hygiene (variable scoping)
- Macro composition
- Conditional compilation
- Macro debugging tools

### Tooling:
- IDE integration
- Syntax highlighting
- Auto-completion
- Refactoring support

## Conclusion

This implementation plan provides a roadmap for completing the LX language's macro system and adding full compilation capabilities. The phased approach ensures each component is thoroughly tested before moving to the next phase.

The end result will be a complete programming language with:
- ✅ Full macro system (definition + expansion)
- ✅ Erlang code generation
- ✅ Beam compilation
- ✅ Production-ready toolchain

This foundation enables LX to be used for real-world applications while maintaining the flexibility and power of its metaprogramming capabilities.
