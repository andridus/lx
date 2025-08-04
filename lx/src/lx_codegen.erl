-module(lx_codegen).

% No include needed for now

-export([compile_direct/1, compile_direct/2, generate_erl/1, generate_erl/2,
         compile_direct_with_specs/2, compile_direct_with_specs/3]).

%% Direct compilation to BEAM
compile_direct(AST) ->
    compile_direct(AST, unknown).

compile_direct(AST, ModuleName) ->
    % Generate Erlang forms
    Forms = ast_to_forms(AST, ModuleName),

    % Compile directly to BEAM
    case compile:forms(Forms, [return, binary, debug_info]) of
        {ok, ModuleName, BeamCode} ->
            {ModuleName, BeamCode, #{}};
        {ok, ModuleName, BeamCode, Warnings} ->
            {ModuleName, BeamCode, #{warnings => Warnings}};
        {error, Errors, Warnings} ->
            {error, {compilation_errors, Errors, Warnings}}
    end.

%% Direct compilation to BEAM with specs
compile_direct_with_specs(AST, Specs) ->
    compile_direct_with_specs(AST, Specs, unknown).

compile_direct_with_specs(AST, Specs, ModuleName) ->
    % Generate Erlang forms with specs
    Forms = ast_to_forms_with_specs(AST, ModuleName, Specs),

    % Compile directly to BEAM
    case compile:forms(Forms, [return, binary, debug_info]) of
        {ok, ModuleName, BeamCode} ->
            {ModuleName, BeamCode, #{}};
        {ok, ModuleName, BeamCode, Warnings} ->
            {ModuleName, BeamCode, #{warnings => Warnings}};
        {error, Errors, Warnings} ->
            {error, {compilation_errors, Errors, Warnings}}
    end.

%% Generate .erl file
generate_erl(AST) ->
    generate_erl(AST, unknown).

generate_erl(AST, ModuleName) ->
    ErlCode = ast_to_erlang_source(AST, ModuleName),
    {ok, lists:flatten(ErlCode)}.

%% Convert AST to Erlang forms
ast_to_forms(AST, ModuleName) ->
    % Generate module form
    ModuleForm = {attribute, 1, module, ModuleName},

    % Generate export form
    ExportForm = {attribute, 1, export, extract_exports(AST)},

    % Generate main function form
    MainFunctionForm = ast_to_main_function(AST),

    % Combine all forms
    [ModuleForm, ExportForm, MainFunctionForm].

%% Convert AST to Erlang forms with specs
ast_to_forms_with_specs(AST, ModuleName, Specs) ->
    % Generate module form
    ModuleForm = {attribute, 1, module, ModuleName},

    % Generate export form
    ExportForm = {attribute, 1, export, extract_exports(AST)},

    % Generate spec forms
    SpecForms = [ast_to_spec_form(Spec) || Spec <- Specs],

    % Generate main function form
    MainFunctionForm = ast_to_main_function(AST),

    % Combine all forms
    [ModuleForm, ExportForm | SpecForms ++ [MainFunctionForm]].

%% Convert spec to Erlang form
ast_to_spec_form({Name, Arity, Type}) ->
    SpecType = type_to_erlang_spec(Type),
    {attribute, 1, spec, {{Name, Arity}, SpecType}}.

%% Convert type to Erlang spec
type_to_erlang_spec({type_const, Name}) ->
    {type, 1, Name, []};
type_to_erlang_spec({type_var, Name}) ->
    {var, 1, Name};
type_to_erlang_spec({type_fun, ParamTypes, ReturnType}) ->
    case ParamTypes of
        [] ->
            ErlReturnType = type_to_erlang_spec(ReturnType),
            ErlReturnType;
        _ ->
            ErlParamTypes = [type_to_erlang_spec(PT) || PT <- ParamTypes],
            ErlReturnType = type_to_erlang_spec(ReturnType),
            {type, 1, '->', [ErlParamTypes, ErlReturnType]}
    end.

%% Extract exports from AST
extract_exports(_AST) ->
    % For now, we'll create a main function that executes all expressions
    [{main, 0}].

% Function form conversion removed for now - we only generate main function

%% Convert block to Erlang expression
block_to_erlang_expr([Expr]) ->
    ast_to_erlang_expr(Expr);
block_to_erlang_expr([Expr | Rest]) ->
    {block, 1, [ast_to_erlang_expr(Expr) | [ast_to_erlang_expr(E) || E <- Rest]]}.

%% Convert AST to main function that executes all expressions
ast_to_main_function(AST) ->
    % Convert all expressions to Erlang expressions
    ErlExprs = [ast_to_erlang_expr(Expr) || Expr <- AST],

    % Create a block with all expressions
    ErlBody = case ErlExprs of
        [SingleExpr] -> SingleExpr;
        MultipleExprs -> {block, 1, MultipleExprs}
    end,

    % Create main function form
    {function, 1, main, 0, [{clause, 1, [], [], [ErlBody]}]}.

%% Convert AST node to Erlang expression
ast_to_erlang_expr({integer, _Line, Value}) ->
    {integer, 1, Value};
ast_to_erlang_expr({float, _Line, Value}) ->
    {float, 1, Value};
ast_to_erlang_expr({string, _Line, Value}) ->
    {bin, 1, [{bin_element, 1, {string, 1, Value}, default, default}]};
ast_to_erlang_expr({atom, _Line, Value}) ->
    {atom, 1, Value};
ast_to_erlang_expr({tuple, _Line, Elements}) ->
    ErlElements = [ast_to_erlang_expr(Elem) || Elem <- Elements],
    {tuple, 1, ErlElements};
ast_to_erlang_expr({list, _Line, Elements}) ->
    ErlElements = [ast_to_erlang_expr(Elem) || Elem <- Elements],
    case ErlElements of
        [] -> {nil, 1};
        _ -> lists:foldr(fun(Elem, Acc) -> {cons, 1, Elem, Acc} end, {nil, 1}, ErlElements)
    end;
ast_to_erlang_expr({map, _Line, Entries}) ->
    ErlEntries = [ast_to_map_entry(Entry) || Entry <- Entries],
    {map, 1, ErlEntries};
ast_to_erlang_expr({binary_op, _Line, Left, Op, Right}) ->
    ErlLeft = ast_to_erlang_expr(Left),
    ErlRight = ast_to_erlang_expr(Right),
    {op, 1, Op, ErlLeft, ErlRight};
ast_to_erlang_expr({macro_def, _Line, _Name, _Params, _Body}) ->
    % Skip macro definitions during code generation
    {atom, 1, ok};
ast_to_erlang_expr({macro_def_infix, _Line, _Name, _Params, _Body}) ->
    % Skip infix macro definitions during code generation
    {atom, 1, ok};
ast_to_erlang_expr({do_block, _Line, ExpressionList}) ->
    % Execute do/end blocks
    block_to_erlang_expr(ExpressionList);
ast_to_erlang_expr(Other) ->
    % Fallback for unknown expressions
    {atom, 1, {unknown_expression, Other}}.

%% Convert map entry to Erlang map entry
ast_to_map_entry({map_entry, _Line, Key, Value}) ->
    ErlKey = ast_to_erlang_expr(Key),
    ErlValue = ast_to_erlang_expr(Value),
    {map_field_assoc, 1, ErlKey, ErlValue}.

%% Convert AST to Erlang source code
ast_to_erlang_source(AST, ModuleName) ->
    % Generate module header
    Header = io_lib:format("-module(~s).~n", [ModuleName]),

    % Generate exports
    Exports = generate_exports_source(AST),

    % Generate main function
    MainFunction = generate_main_function_source(AST),

    % Combine all parts
    Header ++ Exports ++ MainFunction.

%% Generate exports source
generate_exports_source(_AST) ->
    "-export([main/0]).\n\n".

%% Generate main function source
generate_main_function_source(AST) ->
    % Convert all expressions to Erlang source
    ErlExprs = [ast_to_erlang_source_expr(Expr) || Expr <- AST],

    % Create main function body
    Body = case ErlExprs of
        [SingleExpr] -> SingleExpr;
        MultipleExprs -> string:join(MultipleExprs, ",\n    ")
    end,

    io_lib:format("main() ->\n    ~s.\n", [Body]).

%% Convert AST node to Erlang source expression
ast_to_erlang_source_expr({integer, _Line, Value}) ->
    integer_to_list(Value);
ast_to_erlang_source_expr({float, _Line, Value}) ->
    float_to_list(Value);
ast_to_erlang_source_expr({string, _Line, Value}) ->
    "\"" ++ Value ++ "\"";
ast_to_erlang_source_expr({atom, _Line, Value}) ->
    atom_to_list(Value);
ast_to_erlang_source_expr({tuple, _Line, Elements}) ->
    ErlElements = [ast_to_erlang_source_expr(Elem) || Elem <- Elements],
    "{" ++ string:join(ErlElements, ", ") ++ "}";
ast_to_erlang_source_expr({list, _Line, Elements}) ->
    ErlElements = [ast_to_erlang_source_expr(Elem) || Elem <- Elements],
    "[" ++ string:join(ErlElements, ", ") ++ "]";
ast_to_erlang_source_expr({map, _Line, Entries}) ->
    ErlEntries = [ast_to_map_entry_source(Entry) || Entry <- Entries],
    "#{" ++ string:join(ErlEntries, ", ") ++ "}";
ast_to_erlang_source_expr({binary_op, _Line, Left, Op, Right}) ->
    ErlLeft = ast_to_erlang_source_expr(Left),
    ErlRight = ast_to_erlang_source_expr(Right),
    ErlLeft ++ " " ++ atom_to_list(Op) ++ " " ++ ErlRight;
ast_to_erlang_source_expr({macro_def, _Line, _Name, _Params, _Body}) ->
    % Skip macro definitions during code generation
    "ok";
ast_to_erlang_source_expr({macro_def_infix, _Line, _Name, _Params, _Body}) ->
    % Skip infix macro definitions during code generation
    "ok";
ast_to_erlang_source_expr({do_block, _Line, ExpressionList}) ->
    % Execute do/end blocks
    ErlExprs = [ast_to_erlang_source_expr(Expr) || Expr <- ExpressionList],
    string:join(ErlExprs, ",\n    ");
ast_to_erlang_source_expr(Other) ->
    % Fallback for unknown expressions
    io_lib:format("{unknown_expression, ~p}", [Other]).

%% Convert map entry to Erlang source
ast_to_map_entry_source({map_entry, _Line, Key, Value}) ->
    ErlKey = ast_to_erlang_source_expr(Key),
    ErlValue = ast_to_erlang_source_expr(Value),
    ErlKey ++ " => " ++ ErlValue.