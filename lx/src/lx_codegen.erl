-module(lx_codegen).

% No include needed for now

-export([compile_direct/1, compile_direct/2, generate_erl/1, generate_erl/2,
         compile_direct_with_specs/2, compile_direct_with_specs/3,
         ast_to_forms/2, ast_to_forms_with_specs/3, ast_to_erlang_expr/1, form_to_string/1, ast_to_main_function/1, ast_to_erlang_source/2]).

% Compile AST directly to BEAM
compile_direct(AST) ->
    compile_direct(AST, []).

compile_direct(AST, Options) ->
    ModuleName = case Options of
        #{module_name := M} -> M;
        #{module := M} -> M;
        _ -> 'lx_module'
    end,
    Forms = ast_to_forms(AST, ModuleName),
    case compile:forms(Forms, [return_errors]) of
        {ok, ModuleName, Beam} ->
            {ok, Beam};
        {error, Errors, Warnings} ->
            {error, {compilation_errors, Errors, Warnings}}
    end.

% Generate Erlang source code
generate_erl(AST) ->
    generate_erl(AST, []).

generate_erl(AST, Options) ->
    ModuleName = case Options of
        #{module_name := M} -> M;
        #{module := M} -> M;
        _ -> 'lx_module'
    end,
    Forms = ast_to_forms(AST, ModuleName),
    ErlCode = ast_to_erlang_source(Forms, ModuleName),
    {ok, ErlCode}.

% Compile with specs
compile_direct_with_specs(AST, Specs) ->
    compile_direct_with_specs(AST, Specs, []).

compile_direct_with_specs(AST, Specs, Options) ->
    ModuleName = case Options of
        #{module_name := M} -> M;
        #{module := M} -> M;
        _ -> 'lx_module'
    end,
    Forms = ast_to_forms_with_specs(AST, Specs, ModuleName),
    case compile:forms(Forms, [return_errors]) of
        {ok, ModuleName, Beam} ->
            {ok, Beam};
        {error, Errors, Warnings} ->
            {error, {compilation_errors, Errors, Warnings}}
    end.

% Convert AST to Erlang forms
ast_to_forms(AST, ModuleName) ->
    ModuleForm = {attribute, 1, module, ModuleName},
    ExportForm = {attribute, 1, export, extract_exports(AST)},
    MainFunctionForm = ast_to_main_function(AST),
    [ModuleForm, ExportForm, MainFunctionForm].

% Convert AST to forms with specs
ast_to_forms_with_specs(AST, Specs, ModuleName) ->
    ModuleForm = {attribute, 1, module, ModuleName},
    ExportForm = {attribute, 1, export, extract_exports(AST)},
    SpecForms = [ast_to_spec_form(Spec) || Spec <- Specs],
    MainFunctionForm = ast_to_main_function(AST),
    [ModuleForm, ExportForm | SpecForms] ++ [MainFunctionForm].

% Extract exports from AST
extract_exports(_AST) ->
    % For now, we only export main/0
    [{main, 0}].

% Convert AST to main function
ast_to_main_function(AST) ->
    case AST of
        [Expr] ->
            % Single expression
            Body = ast_to_erlang_expr(Expr),
            {function, 1, main, 0, [{clause, 1, [], [], [Body]}]};
        Exprs when is_list(Exprs) ->
            % Multiple expressions - create a block with all expressions executed
            Bodies = [ast_to_erlang_expr(Expr) || Expr <- Exprs],
            % Execute all expressions and return the last one
            {function, 1, main, 0, [{clause, 1, [], [], Bodies}]};
        SingleExpr ->
            % Single expression (not in a list)
            Body = ast_to_erlang_expr(SingleExpr),
            {function, 1, main, 0, [{clause, 1, [], [], [Body]}]}
    end.

% Convert AST to Erlang expression
ast_to_erlang_expr({integer, _Line, Value}) ->
    {integer, 1, Value};
ast_to_erlang_expr({float, _Line, Value}) ->
    {float, 1, Value};
ast_to_erlang_expr({string, _Line, Value}) ->
    {bin, 1, [{bin_element, 1, {string, 1, Value}, default, default}]};
ast_to_erlang_expr({atom, _Line, Value}) ->
    {atom, 1, Value};
ast_to_erlang_expr({ident, _Line, Name}) ->
    % Convert ident to atom for Erlang
    {atom, 1, list_to_atom(Name)};
ast_to_erlang_expr({list, _Line, Elements}) ->
    FlatElements = ensure_list_elements(Elements),
    ErlElements = [ast_to_erlang_expr(Elem) || Elem <- FlatElements],
    case ErlElements of
        [] -> {nil, 1};
        _ -> lists:foldr(fun(Elem, Acc) -> {cons, 1, Elem, Acc} end, {nil, 1}, ErlElements)
    end;
ast_to_erlang_expr({map, _Line, Entries}) ->
    ErlEntries = [ast_to_map_entry(Entry) || Entry <- Entries],
    {map, 1, ErlEntries};
ast_to_erlang_expr({tuple, Line, [{atom, _, op}, {integer, _, LineNum}, {atom, _, Op}, Left, Right]}) ->
    ErlLeft = ast_to_erlang_expr(Left),
    ErlRight = ast_to_erlang_expr(Right),
    {op, LineNum, Op, ErlLeft, ErlRight};
ast_to_erlang_expr({tuple, Line, Elements}) ->
    ErlElements = [ast_to_erlang_expr(Elem) || Elem <- Elements],
    {tuple, Line, ErlElements};
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
% Convert AST to Erlang expression - reconhece estrutura padronizada
ast_to_erlang_expr({FunName, Meta, Args}) when is_atom(FunName), is_map(Meta) ->
    % Verifica se é um operador infix
    case maps:get(infix, Meta, false) of
        true ->
            % Operador infix: left op right
            case Args of
                [Left, Right] ->
                    ErlLeft = ast_to_erlang_expr(Left),
                    ErlRight = ast_to_erlang_expr(Right),
                    {op, 1, FunName, ErlLeft, ErlRight};
                _ ->
                    {atom, 1, {invalid_infix_args, FunName, Args}}
            end;
        false ->
            % Função normal: fun_name(args)
            case is_list(Args) of
                true ->
                    ErlArgs = [ast_to_erlang_expr(Arg) || Arg <- Args],
                    {call, 1, {atom, 1, FunName}, ErlArgs};
                false ->
                    % Args não é uma lista, trata como argumento único
                    ErlArg = ast_to_erlang_expr(Args),
                    {call, 1, {atom, 1, FunName}, [ErlArg]}
            end
    end;
ast_to_erlang_expr(Other) ->
    % Fallback for unknown expressions
    {atom, 1, {unknown_expression, Other}}.

% Convert map entries to maps
map_entries_to_maps([]) ->
    #{};
map_entries_to_maps([{map_entry, _Line, Key, Value} | Rest]) ->
    KeyAtom = case Key of
        {atom, _, K} -> K;
        _ -> Key
    end,
    ValueAtom = case Value of
        {atom, _, V} -> V;
        {integer, _, V} -> V;
        {string, _, V} -> V;
        _ -> Value
    end,
    maps:put(KeyAtom, ValueAtom, map_entries_to_maps(Rest)).

% Convert do block to Erlang expression
block_to_erlang_expr([]) ->
    {atom, 1, ok};
block_to_erlang_expr([LastExpr]) ->
    ast_to_erlang_expr(LastExpr);
block_to_erlang_expr([Expr | Rest]) ->
    ErlExpr = ast_to_erlang_expr(Expr),
    ErlRest = block_to_erlang_expr(Rest),
    {block, 1, [ErlExpr, ErlRest]}.

% Convert map entry to Erlang
ast_to_map_entry({map_entry, Line, Key, Value}) ->
    ErlKey = ast_to_erlang_expr(Key),
    ErlValue = ast_to_erlang_expr(Value),
    {map_field_assoc, Line, ErlKey, ErlValue};
ast_to_map_entry({Key, Value}) ->
    ErlKey = ast_to_erlang_expr(Key),
    ErlValue = ast_to_erlang_expr(Value),
    {map_field_assoc, 1, ErlKey, ErlValue}.

% Convert spec to form
ast_to_spec_form(Spec) ->
    % This would convert a spec AST to an Erlang spec form
    % For now, return a placeholder
    {attribute, 1, spec, Spec}.

% Convert forms to Erlang source code
ast_to_erlang_source(Forms, ModuleName) ->
    % Convert forms to string representation
    FormsStr = [form_to_string(Form) || Form <- Forms],
    string:join(FormsStr, "\n").

% Convert form to string
form_to_string({attribute, Line, module, ModuleName}) ->
    io_lib:format("-module(~p).", [ModuleName]);
form_to_string({attribute, Line, export, Exports}) ->
    ExportStr = string:join([io_lib:format("~p/~p", [Fun, Arity]) || {Fun, Arity} <- Exports], ", "),
    io_lib:format("-export([~s]).", [ExportStr]);
form_to_string({function, Line, Name, Arity, Clauses}) ->
    ClausesStr = string:join([clause_to_string(Clause) || Clause <- Clauses], ";\n    "),
    case Arity of
        0 -> io_lib:format("~p() ->\n    ~s.", [Name, ClausesStr]);
        _ -> io_lib:format("~p(~p) ->\n    ~s.", [Name, Arity, ClausesStr])
    end;
form_to_string(Other) ->
    io_lib:format("% Unknown form: ~p", [Other]).

% Convert clause to string
clause_to_string({clause, Line, Patterns, Guards, Body}) ->
    PatternsStr = case Patterns of
        [] -> "";
        _ -> string:join([pattern_to_string(Pattern) || Pattern <- Patterns], ", ")
    end,
    GuardsStr = case Guards of
        [] -> "";
        _ -> " when " ++ string:join([guard_to_string(Guard) || Guard <- Guards], ", ")
    end,
    BodyStr = string:join([expr_to_string(Expr) || Expr <- Body], ", "),
    io_lib:format("~s~s~s", [PatternsStr, GuardsStr, BodyStr]).

% Convert pattern to string
pattern_to_string({var, Line, Name}) ->
    atom_to_list(Name);
pattern_to_string({integer, Line, Value}) ->
    integer_to_list(Value);
pattern_to_string({atom, Line, Value}) ->
    atom_to_list(Value);
pattern_to_string(Other) ->
    io_lib:format("~p", [Other]).

% Convert guard to string
guard_to_string(Guard) ->
    expr_to_string(Guard).

% Convert expression to string
expr_to_string({integer, Line, Value}) ->
    integer_to_list(Value);
expr_to_string({atom, Line, Value}) ->
    atom_to_list(Value);
expr_to_string({op, Line, Op, Left, Right}) ->
    LeftStr = expr_to_string(Left),
    RightStr = expr_to_string(Right),
    OpStr = case Op of
        Op when is_atom(Op) -> atom_to_list(Op);
        Op when is_list(Op) -> Op;
        _ -> io_lib:format("~p", [Op])
    end,
    io_lib:format("~s ~s ~s", [LeftStr, OpStr, RightStr]);
expr_to_string({call, Line, {atom, _, FunName}, Args}) ->
    ArgsStr = string:join([expr_to_string(Arg) || Arg <- Args], ", "),
    io_lib:format("~s(~s)", [atom_to_list(FunName), ArgsStr]);
expr_to_string({tuple, Line, Elements}) ->
    ElementsStr = string:join([expr_to_string(Elem) || Elem <- Elements], ", "),
    io_lib:format("{~s}", [ElementsStr]);
expr_to_string({cons, Line, Head, Tail}) ->
    HeadStr = expr_to_string(Head),
    TailStr = expr_to_string(Tail),
    io_lib:format("[~s | ~s]", [HeadStr, TailStr]);
expr_to_string({nil, Line}) ->
    "[]";
expr_to_string({map, Line, Entries}) ->
    EntriesStr = string:join([map_entry_to_string(Entry) || Entry <- Entries], ", "),
    io_lib:format("#{~s}", [EntriesStr]);
expr_to_string(Other) ->
    io_lib:format("~p", [Other]).

% Convert map entry to string
map_entry_to_string({map_field_assoc, Line, Key, Value}) ->
    KeyStr = expr_to_string(Key),
    ValueStr = expr_to_string(Value),
    io_lib:format("~s => ~s", [KeyStr, ValueStr]).

ensure_list_elements({list, _, Els}) -> ensure_list_elements(Els);
ensure_list_elements(Els) when is_list(Els) ->
    lists:flatmap(fun ensure_list_elements/1, Els);
ensure_list_elements(Other) -> [Other].