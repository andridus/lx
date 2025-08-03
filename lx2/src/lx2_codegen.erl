-module(lx2_codegen).

-include("lx2.hrl").

-export([compile_direct/1, compile_direct/2, generate_erl/1, generate_erl/2, generate_erl_with_specs/3, compile_direct_with_specs/2, compile_direct_with_specs/3]).

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
    generate_erl_with_specs(AST, ModuleName, []).

%% Generate .erl file with specs
generate_erl_with_specs(AST, ModuleName, Specs) ->
    ErlCode = ast_to_erlang_source_with_specs(AST, ModuleName, Specs),
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

%% Convert AST to Erlang forms with specs
ast_to_forms_with_specs(AST, ModuleName, Specs) ->
    % Generate module form
    ModuleForm = {attribute, 1, module, ModuleName},

    % Generate export form
    ExportForm = {attribute, 1, export, extract_exports(AST)},

    % Generate spec forms
    SpecForms = [ast_to_spec_form(Spec) || Spec <- Specs],

    % Generate function forms
    FunctionForms = [ast_to_function_form(Fun) || Fun <- AST],

    % Combine all forms
    [ModuleForm, ExportForm | SpecForms ++ FunctionForms].

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
    end;
type_to_erlang_spec({forall, Vars, Type}) ->
    ErlType = type_to_erlang_spec(Type),
    {type, 1, 'fun', [{type, 1, product, [{var, 1, Var} || Var <- Vars]}, ErlType]}.

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

%% Convert variable name to Erlang format (capitalized + hash)
var_to_erlang(Var) ->
    VarStr = atom_to_list(Var),
    Capitalized = string:to_upper(VarStr),
    list_to_atom(Capitalized ++ "_" ++ integer_to_list(erlang:phash2(VarStr, 1000000))).

%% Convert AST node to Erlang expression
ast_to_erlang_expr({literal, Type, Value}) ->
    case Type of
        integer -> {integer, 1, Value};
        float -> {float, 1, Value};
        string -> {bin, 1, [{bin_element, 1, {string, 1, Value}, default, default}]};
        atom -> {atom, 1, Value};
        boolean -> {atom, 1, Value};
        nil -> {atom, 1, nil}
    end;
ast_to_erlang_expr({variable_binding, Var, Expr}) ->
    ErlVar = var_to_erlang(Var),
    {match, 1, {var, 1, ErlVar}, ast_to_erlang_expr(Expr)};
ast_to_erlang_expr({variable_ref, Var}) ->
    ErlVar = var_to_erlang(Var),
    {var, 1, ErlVar}.

%% Convert AST to Erlang source code with specs
ast_to_erlang_source_with_specs(AST, ModuleName, Specs) ->
    % Generate module header
    Header = io_lib:format("-module(~s).~n", [ModuleName]),

    % Generate exports
    Exports = generate_exports_source(AST),

    % Generate specs
    SpecsCode = generate_specs_source(Specs),

    % Generate functions
    Functions = generate_functions_source(AST),

    % Combine all parts
    Header ++ Exports ++ SpecsCode ++ Functions.

%% Generate specs source
generate_specs_source([]) ->
    "";
generate_specs_source(Specs) ->
    SpecStrings = [generate_spec_source(Spec) || Spec <- Specs],
    string:join(SpecStrings, "\n") ++ "\n\n".

%% Generate single spec source
generate_spec_source({Name, Arity, Type}) ->
    SpecType = type_to_erlang_spec_source(Type),
    case Arity of
        0 ->
            io_lib:format("-spec ~s() -> ~s.", [Name, SpecType]);
        _ ->
            io_lib:format("-spec ~s(~s) -> ~s.", [Name, string:join(lists:duplicate(Arity, "_"), ", "), SpecType])
    end.

%% Convert type to Erlang spec source
type_to_erlang_spec_source({type_const, Name}) ->
    atom_to_list(Name) ++ "()";
type_to_erlang_spec_source({type_var, Name}) ->
    atom_to_list(Name);
type_to_erlang_spec_source({type_fun, ParamTypes, ReturnType}) ->
    case ParamTypes of
        [] ->
            ErlReturnType = type_to_erlang_spec_source(ReturnType),
            ErlReturnType;
        _ ->
            ErlParamTypes = [type_to_erlang_spec_source(PT) || PT <- ParamTypes],
            ErlReturnType = type_to_erlang_spec_source(ReturnType),
            "(" ++ string:join(ErlParamTypes, ", ") ++ ") -> " ++ ErlReturnType
    end;
type_to_erlang_spec_source({forall, Vars, Type}) ->
    ErlType = type_to_erlang_spec_source(Type),
    "fun((" ++ string:join([atom_to_list(Var) || Var <- Vars], ", ") ++ ") -> " ++ ErlType ++ ")".

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
    end;
ast_to_erlang_source_expr({variable_binding, Var, Expr}) ->
    ErlVar = var_to_erlang(Var),
    atom_to_list(ErlVar) ++ " = " ++ ast_to_erlang_source_expr(Expr);
ast_to_erlang_source_expr({variable_ref, Var}) ->
    ErlVar = var_to_erlang(Var),
    atom_to_list(ErlVar).