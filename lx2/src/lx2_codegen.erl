-module(lx2_codegen).

-include("lx2.hrl").

-export([compile_direct/1, compile_direct/2, generate_erl/1, generate_erl/2]).

%% Direct compilation to BEAM
compile_direct(AST) ->
    compile_direct(AST, lx2_module).

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

%% Generate .erl file
generate_erl(AST) ->
    generate_erl(AST, lx2_module).

generate_erl(AST, ModuleName) ->
    ErlCode = ast_to_erlang_source(AST, ModuleName),
    {ok, lists:flatten(ErlCode)}.

%% Convert AST to Erlang forms
ast_to_forms(AST, ModuleName) ->
    % Generate module form
    ModuleForm = {attribute, 1, module, ModuleName},

    % Generate export form
    ExportForm = {attribute, 1, export, extract_exports(AST)},

    % Generate function forms
    FunctionForms = [ast_to_function_form(Fun) || Fun <- AST],

    % Combine all forms
    [ModuleForm, ExportForm | FunctionForms].

%% Extract exports from AST
extract_exports(AST) ->
    [{Name, length(Params)} || {function_def, Name, Params, _} <- AST].

%% Convert function AST to Erlang form
ast_to_function_form({function_def, Name, Params, Body}) ->
    % Convert parameters to Erlang variables
    ErlParams = [list_to_atom("P" ++ integer_to_list(I)) || I <- lists:seq(1, length(Params))],

    % Convert body to Erlang expression
    ErlBody = block_to_erlang_expr(Body),

    % Create function form
    {function, 1, Name, length(Params), [{clause, 1, ErlParams, [], [ErlBody]}]}.

%% Convert block to Erlang expression
block_to_erlang_expr([Expr]) ->
    ast_to_erlang_expr(Expr);
block_to_erlang_expr([Expr | Rest]) ->
    {block, 1, [ast_to_erlang_expr(Expr) | [ast_to_erlang_expr(E) || E <- Rest]]}.

%% Convert AST node to Erlang expression
ast_to_erlang_expr({literal, Type, Value}) ->
    case Type of
        integer -> {integer, 1, Value};
        float -> {float, 1, Value};
        string -> {bin, 1, [{bin_element, 1, {string, 1, Value}, default, default}]};
        atom -> {atom, 1, Value};
        boolean -> {atom, 1, Value};
        nil -> {atom, 1, nil}
    end.

%% Convert AST to Erlang source code
ast_to_erlang_source(AST, ModuleName) ->
    % Generate module header
    Header = io_lib:format("-module(~s).~n", [ModuleName]),

    % Generate exports
    Exports = generate_exports_source(AST),

    % Generate functions
    Functions = generate_functions_source(AST),

    % Combine all parts
    Header ++ Exports ++ Functions.

%% Generate exports source
generate_exports_source(AST) ->
    Exports = extract_exports(AST),
    ExportStrings = [io_lib:format("    ~s/~p", [Name, Arity]) || {Name, Arity} <- Exports],
    "-export([\n" ++ string:join(ExportStrings, ",\n") ++ "\n]).\n\n".

%% Generate functions source
generate_functions_source(AST) ->
    [generate_function_source(Fun) || Fun <- AST].

%% Generate function source
generate_function_source({function_def, Name, Params, Body}) ->
    ErlParams = [list_to_atom("P" ++ integer_to_list(I)) || I <- lists:seq(1, length(Params))],
    ErlBody = block_to_erlang_source(Body),

    io_lib:format("~s(~s) ->\n    ~s.\n\n", [
        atom_to_list(Name),
        string:join([atom_to_list(P) || P <- ErlParams], ", "),
        ErlBody
    ]).

%% Convert block to Erlang source
block_to_erlang_source([Expr]) ->
    ast_to_erlang_source_expr(Expr);
block_to_erlang_source([Expr | Rest]) ->
    ExprStr = ast_to_erlang_source_expr(Expr),
    RestStr = string:join([ast_to_erlang_source_expr(E) || E <- Rest], ",\n    "),
    ExprStr ++ ",\n    " ++ RestStr.

%% Convert AST node to Erlang source expression
ast_to_erlang_source_expr({literal, Type, Value}) ->
    case Type of
        integer -> integer_to_list(Value);
        float -> float_to_list(Value);
        string -> "\"" ++ Value ++ "\"";
        atom -> atom_to_list(Value);
        boolean -> atom_to_list(Value);
        nil -> "nil"
    end.