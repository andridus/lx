# LX Language - Macro Expansion Tasks

## Task List for Macro Expansion and Compilation Implementation

### Phase 1: Macro Call Recognition

#### Task 1.1: Extend Lexer for Macro Calls
**File**: `lx/src/lx_lexer.xrl`
**Priority**: High
**Estimated Time**: 2 hours

**Description**: Add support for recognizing macro calls in the lexer.

**Implementation**:
```erlang
% Add to Rules section
macro_call_start : {token, {macro_call_start, TokenLine}}.
macro_call_end : {token, {macro_call_end, TokenLine}}.
```

**Test Cases**:
- `a [1,2,3]` should tokenize as macro call
- `b do ... end` should tokenize as macro call with block
- `c(x, y)` should tokenize as macro call with arguments

**Acceptance Criteria**:
- [ ] Macro calls are correctly tokenized
- [ ] No conflicts with existing tokens
- [ ] All test cases pass

---

#### Task 1.2: Add Parser Rules for Macro Calls
**File**: `lx/src/lx_parser.yrl`
**Priority**: High
**Estimated Time**: 4 hours

**Description**: Add grammar rules to parse different types of macro calls.

**Implementation**:
```erlang
% Add to Nonterminals section
macro_call
macro_call_arguments

% Add to Rules section
expression -> macro_call : '$1'.

macro_call -> atom '(' macro_call_arguments ')' :
    {macro_call, line('$1'), element(3, '$1'), '$3'}.

macro_call -> atom do_block :
    {macro_call_block, line('$1'), element(3, '$1'), '$2'}.

macro_call -> atom expression :
    {macro_call_simple, line('$1'), element(3, '$1'), '$2'}.

macro_call_arguments -> expression : ['$1'].
macro_call_arguments -> expression ',' macro_call_arguments : ['$1' | '$3'].
macro_call_arguments -> : [].
```

**Test Cases**:
- `a [1,2,3]` → `{macro_call_simple, line, a, [1,2,3]}`
- `b do x=1; y=2 end` → `{macro_call_block, line, b, do_block}`
- `c(x, y)` → `{macro_call, line, c, [x, y]}`

**Acceptance Criteria**:
- [ ] All macro call patterns are parsed correctly
- [ ] AST structure is consistent
- [ ] Error handling for invalid syntax

---

### Phase 2: Macro Expansion Engine

#### Task 2.1: Enhance Macro Registry
**File**: `lx/src/lx_macros.erl`
**Priority**: High
**Estimated Time**: 6 hours

**Description**: Extend the macro registry to support macro lookup and expansion.

**Implementation**:
```erlang
% Add new functions
expand_macro_call(Name, Arguments, Registry) ->
    case get_macro(Name, Registry) of
        {Metadata, Body} ->
            expand_macro_body(Body, Arguments, Metadata);
        not_found ->
            {error, {undefined_macro, Name}}
    end.

expand_macro_body(Body, Arguments, {Arity, _, _, Modifiers}) ->
    case length(Arguments) of
        Arity ->
            substitute_arguments(Body, Arguments);
        _ ->
            {error, {wrong_arity, Arity, length(Arguments)}}
    end.
```

**Test Cases**:
- Lookup existing macro → returns macro body
- Lookup non-existent macro → returns error
- Wrong argument count → returns error

**Acceptance Criteria**:
- [ ] Macro lookup works correctly
- [ ] Argument validation is implemented
- [ ] Error messages are clear and helpful

---

#### Task 2.2: Implement Argument Substitution
**File**: `lx/src/lx_macros.erl`
**Priority**: High
**Estimated Time**: 8 hours

**Description**: Implement parameter substitution in macro bodies.

**Implementation**:
```erlang
substitute_arguments(Body, Arguments) ->
    substitute_in_ast(Body, Arguments).

substitute_in_ast({atom, Line, body}, [Arg | _]) ->
    Arg;
substitute_in_ast({atom, Line, args}, Arguments) ->
    {list, Line, Arguments};
substitute_in_ast(Node, Arguments) when is_tuple(Node) ->
    list_to_tuple([substitute_in_ast(Elem, Arguments) || Elem <- tuple_to_list(Node)]);
substitute_in_ast(Node, _) ->
    Node.
```

**Test Cases**:
- `body` parameter → replaced with first argument
- `args` parameter → replaced with argument list
- Nested structures → recursively substituted
- Non-parameter atoms → unchanged

**Acceptance Criteria**:
- [ ] Parameter substitution works correctly
- [ ] Nested structures are handled
- [ ] Non-parameters are preserved

---

#### Task 2.3: Build Expansion Pipeline
**File**: `lx/src/lx_compiler.erl`
**Priority**: High
**Estimated Time**: 4 hours

**Description**: Update the compiler to include macro expansion in the pipeline.

**Implementation**:
```erlang
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

register_all_macros(AST) ->
    lists:foldl(fun register_macro_if_needed/2, lx_macros:init(), AST).

expand_all_macros(AST, Registry) ->
    [expand_node_if_macro_call(Node, Registry) || Node <- AST].
```

**Test Cases**:
- Macro definitions are registered
- Macro calls are expanded
- Non-macro nodes are preserved
- Error handling works correctly

**Acceptance Criteria**:
- [ ] Two-pass compilation works
- [ ] Macro registration is complete
- [ ] Expansion is applied to all calls

---

### Phase 3: Erlang Code Generation

#### Task 3.1: Create Erlang Generator Module
**File**: `lx/src/lx_erlang_generator.erl`
**Priority**: Medium
**Estimated Time**: 6 hours

**Description**: Create a new module for generating Erlang code from LX AST.

**Implementation**:
```erlang
-module(lx_erlang_generator).
-export([generate/1, generate_file/2]).

generate(AST) ->
    generate_module(AST).

generate_module(AST) ->
    ModuleName = extract_module_name(AST),
    Functions = extract_functions(AST),
    generate_erlang_module(ModuleName, Functions).

generate_erlang_module(ModuleName, Functions) ->
    ModuleHeader = generate_module_header(ModuleName),
    FunctionCode = [generate_function(F) || F <- Functions],
    [ModuleHeader, "\n", FunctionCode].
```

**Test Cases**:
- Empty AST → generates basic module
- AST with functions → generates complete module
- Invalid AST → returns error

**Acceptance Criteria**:
- [ ] Module structure is correct
- [ ] Function generation works
- [ ] Error handling is implemented

---

#### Task 3.2: Implement AST to Erlang Translation
**File**: `lx/src/lx_erlang_generator.erl`
**Priority**: Medium
**Estimated Time**: 8 hours

**Description**: Implement translation rules from LX AST to Erlang code.

**Implementation**:
```erlang
generate_function({macro_def, Line, Name, Params, Body}) ->
    ParamList = generate_params(Params),
    BodyCode = generate_expression(Body),
    io_lib:format("~s(~s) ->\n    ~s.\n", [Name, ParamList, BodyCode]).

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

**Test Cases**:
- Tuples → `{a, b, c}`
- Lists → `[1, 2, 3]`
- Atoms → `atom`
- Integers → `123`
- Complex expressions → nested structures

**Acceptance Criteria**:
- [ ] All AST nodes are translated
- [ ] Generated code is valid Erlang
- [ ] Complex structures are handled

---

#### Task 3.3: Add File Generation
**File**: `lx/src/lx_erlang_generator.erl`
**Priority**: Medium
**Estimated Time**: 2 hours

**Description**: Add capability to write generated Erlang code to files.

**Implementation**:
```erlang
generate_file(AST, Filename) ->
    ErlangCode = generate(AST),
    ErlFilename = filename:rootname(Filename, ".lx") ++ ".erl",
    case file:write_file(ErlFilename, ErlangCode) of
        ok -> {ok, ErlFilename};
        {error, Reason} -> {error, Reason}
    end.
```

**Test Cases**:
- Valid AST → creates .erl file
- Invalid filename → returns error
- File system issues → handled gracefully

**Acceptance Criteria**:
- [ ] Files are created correctly
- [ ] Filename transformation works
- [ ] Error handling is robust

---

### Phase 4: Beam Compilation

#### Task 4.1: Integrate with Erlang Compiler
**File**: `lx/src/lx_compiler.erl`
**Priority**: Medium
**Estimated Time**: 4 hours

**Description**: Add beam compilation capability using Erlang's compile module.

**Implementation**:
```erlang
compile_to_beam(Source, Options) ->
    case compile(Source) of
        {ok, AST} ->
            case lx_erlang_generator:generate(AST) of
                {ok, ErlCode} ->
                    compile_erlang_to_beam(ErlCode, Options);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

compile_erlang_to_beam(ErlCode, Options) ->
    case compile:forms(ErlCode, Options) of
        {ok, ModuleName, BeamCode} ->
            BeamFilename = atom_to_list(ModuleName) ++ ".beam",
            case file:write_file(BeamFilename, BeamCode) of
                ok -> {ok, BeamFilename};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

**Test Cases**:
- Valid Erlang code → generates .beam file
- Invalid code → returns compilation error
- Compilation options → applied correctly

**Acceptance Criteria**:
- [ ] Beam files are generated
- [ ] Compilation errors are handled
- [ ] Options are respected

---

#### Task 4.2: Extend CLI for Compilation
**File**: `lx/src/lx_cli.erl`
**Priority**: Medium
**Estimated Time**: 3 hours

**Description**: Add new CLI commands for code generation and compilation.

**Implementation**:
```erlang
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
    end;

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
    end.
```

**Test Cases**:
- `lx-compiler generate file.lx` → generates .erl file
- `lx-compiler compile file.lx` → generates .beam file
- Invalid options → shows usage help

**Acceptance Criteria**:
- [ ] New commands work correctly
- [ ] Help messages are clear
- [ ] Error handling is user-friendly

---

### Phase 5: Testing and Integration

#### Task 5.1: Create Comprehensive Test Suite
**Files**: `lx/test/`
**Priority**: High
**Estimated Time**: 6 hours

**Description**: Create unit and integration tests for all new functionality.

**Test Categories**:
- Macro call parsing tests
- Macro expansion tests
- Erlang generation tests
- Beam compilation tests
- End-to-end workflow tests

**Implementation**:
```erlang
% Example test
test_macro_expansion() ->
    Source = "defmacro a(x) do x end\na(5)",
    {ok, AST} = lx_compiler:compile(Source),
    ?assertMatch([{macro_def, _, a, _, _}, {integer, _, 5}], AST).
```

**Acceptance Criteria**:
- [ ] All functionality is tested
- [ ] Edge cases are covered
- [ ] Tests pass consistently

---

#### Task 5.2: Update Makefile
**File**: `lx/Makefile`
**Priority**: Low
**Estimated Time**: 1 hour

**Description**: Add new targets for testing and compilation.

**Implementation**:
```makefile
test-macro-expansion: compile
	./_build/default/bin/lx-compiler generate examples/macro_test.lx
	./_build/default/bin/lx-compiler compile examples/macro_test.lx

test-full-pipeline: compile
	./_build/default/bin/lx-compiler run examples/macro_test.lx
	./_build/default/bin/lx-compiler generate examples/macro_test.lx
	./_build/default/bin/lx-compiler compile examples/macro_test.lx
```

**Acceptance Criteria**:
- [ ] New targets work correctly
- [ ] Integration with existing targets
- [ ] Clear target descriptions

---

## Implementation Timeline

### Week 1: Foundation
- [ ] Task 1.1: Extend Lexer for Macro Calls
- [ ] Task 1.2: Add Parser Rules for Macro Calls
- [ ] Task 2.1: Enhance Macro Registry

### Week 2: Core Engine
- [ ] Task 2.2: Implement Argument Substitution
- [ ] Task 2.3: Build Expansion Pipeline
- [ ] Task 3.1: Create Erlang Generator Module

### Week 3: Code Generation
- [ ] Task 3.2: Implement AST to Erlang Translation
- [ ] Task 3.3: Add File Generation
- [ ] Task 4.1: Integrate with Erlang Compiler

### Week 4: Integration and Testing
- [ ] Task 4.2: Extend CLI for Compilation
- [ ] Task 5.1: Create Comprehensive Test Suite
- [ ] Task 5.2: Update Makefile

## Success Criteria

### Phase 1 Success:
- [ ] Macro calls are correctly parsed
- [ ] AST structure is consistent
- [ ] No parsing conflicts

### Phase 2 Success:
- [ ] Macro expansion works correctly
- [ ] Argument substitution is accurate
- [ ] Error handling is robust

### Phase 3 Success:
- [ ] Erlang code is generated correctly
- [ ] Generated code is valid Erlang
- [ ] File generation works

### Phase 4 Success:
- [ ] Beam files are created
- [ ] Compilation pipeline works
- [ ] CLI commands function

### Overall Success:
- [ ] End-to-end workflow functions
- [ ] All tests pass
- [ ] Documentation is complete
- [ ] Performance is acceptable

## Risk Mitigation

### Technical Risks:
- **Parser conflicts**: Monitor shift/reduce conflicts
- **Performance issues**: Profile large files
- **Memory leaks**: Test with large ASTs

### Timeline Risks:
- **Scope creep**: Stick to defined tasks
- **Integration issues**: Test early and often
- **Dependency delays**: Have fallback plans

## Next Steps After Completion

1. **Performance optimization**
2. **Advanced macro features**
3. **IDE integration**
4. **Community documentation**
5. **Real-world testing**