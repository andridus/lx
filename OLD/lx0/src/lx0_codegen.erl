-module(lx0_codegen).
-include("../include/lx0.hrl").

-export([generate/1, generate/2]).

%% Generate Erlang code from AST
generate(AST) ->
    generate(AST, 'basic').

%% Generate Erlang code from AST with module name
generate(AST, ModuleName) ->
    Records = extract_records(AST),
    Types = extract_types(AST),
    Functions = extract_functions(AST),

    generate_module(ModuleName, Records, Types, Functions).

%% Extract module name from AST (use first function name or default)
extract_module_name(AST) ->
    case find_first_function(AST) of
        {function_def, _, {function_head, {var, _, Name}, _, _}, _, _} ->
            list_to_atom(atom_to_list(Name));
        _ ->
            'basic'
    end.

find_first_function([{function_def, _, Head, _, _} | _]) ->
    {function_def, undefined, Head, undefined, undefined};
find_first_function([_ | Rest]) ->
    find_first_function(Rest);
find_first_function([]) ->
    not_found.

%% Extract record definitions from AST
extract_records(AST) ->
    lists:filtermap(fun extract_record/1, AST).

extract_record({record_def, Name, Fields}) ->
    {true, {record_def, Name, Fields}};
extract_record(_) ->
    false.

%% Extract type definitions from AST
extract_types(AST) ->
    lists:filtermap(fun extract_type/1, AST).

extract_type({type_def, Name, Spec}) ->
    {true, {type_def, Name, Spec}};
extract_type(_) ->
    false.

%% Extract function definitions from AST
extract_functions(AST) ->
    lists:filtermap(fun extract_function/1, AST).

extract_function({function_def, Head, Body, Visibility}) ->
    {true, {function_def, Head, Body, Visibility}};
extract_function(_) ->
    false.

%% Generate complete Erlang module
generate_module(ModuleName, Records, Types, Functions) ->
    ModuleHeader = generate_module_header(ModuleName, Functions),
    RecordDefs = generate_record_definitions(Records),
    TypeSpecs = generate_type_specifications(Types),
    FunctionDefs = generate_function_definitions(Functions),

    lists:flatten([
        ModuleHeader,
        "\n\n",
        string:join(RecordDefs, "\n"),
        "\n\n",
        string:join(TypeSpecs, "\n"),
        "\n\n",
        string:join(FunctionDefs, "\n")
    ]).

%% Generate module header
generate_module_header(ModuleName) ->
    io_lib:format("-module(~p).\n-export([]).", [ModuleName]).

%% Generate module header with exports
generate_module_header(ModuleName, Functions) ->
    Exports = generate_exports(Functions),
    io_lib:format("-module(~p).\n-export([~s]).", [ModuleName, Exports]).

%% Generate record definitions
generate_record_definitions(Records) ->
    lists:map(fun generate_record_definition/1, Records).

generate_record_definition({record_def, {var, _, Name}, Fields}) ->
    FieldDefs = lists:map(fun generate_record_field/1, Fields),
    LowerName = list_to_atom(string:to_lower(atom_to_list(Name))),
    io_lib:format("-record(~p, {~s}).", [LowerName, string:join(FieldDefs, ", ")]).

generate_record_field({record_field, {var, _, Name}, Type}) ->
    io_lib:format("~p", [Name]).

%% Generate type specifications
generate_type_specifications(Types) ->
    lists:map(fun generate_type_specification/1, Types).

generate_type_specification({type_def, Name, Spec}) ->
    TypeStr = generate_type_spec(Spec),
    io_lib:format("-type ~p() :: ~s.", [Name, TypeStr]).

generate_type_spec({type_spec, Type}) ->
    atom_to_list(Type);
generate_type_spec(list) ->
    "list()";
generate_type_spec({list, Type}) ->
    io_lib:format("[~s]", [generate_type_spec(Type)]);
generate_type_spec(tuple) ->
    "tuple()";
generate_type_spec({tuple, Types}) ->
    TypeStrs = lists:map(fun generate_type_spec/1, Types),
    io_lib:format("{~s}", [string:join(TypeStrs, ", ")]);
generate_type_spec({union, Type1, Type2}) ->
    io_lib:format("~s | ~s", [generate_type_spec(Type1), generate_type_spec(Type2)]).

%% Generate function definitions
generate_function_definitions(Functions) ->
    lists:map(fun generate_function_definition/1, Functions).

generate_function_definition({function_def, Head, Body, Visibility}) ->
    {FunctionName, Args, TypeSpec} = generate_function_head(Head),
    BodyCode = generate_expression(Body),

    case TypeSpec of
        undefined ->
            io_lib:format("~s(~s) ->\n    ~s.", [FunctionName, Args, BodyCode]);
        _ ->
            TypeSpecStr = generate_type_spec(TypeSpec),
            io_lib:format("-spec ~s(~s) -> ~s.\n~s(~s) ->\n    ~s.",
                         [FunctionName, Args, TypeSpecStr, FunctionName, Args, BodyCode])
    end.

generate_function_head({function_head, {var, _, Name}, Args}) ->
    ArgsStr = generate_pattern_list(Args),
    {string:to_lower(atom_to_list(Name)), ArgsStr, undefined};
generate_function_head({function_head, {var, _, Name}, Args, TypeSpec}) ->
    ArgsStr = generate_pattern_list(Args),
    {string:to_lower(atom_to_list(Name)), ArgsStr, TypeSpec}.

generate_pattern_list([]) ->
    "";
generate_pattern_list(Patterns) ->
    PatternStrs = lists:map(fun generate_pattern/1, Patterns),
    string:join(PatternStrs, ", ").

%% Generate expressions
generate_expression({literal, Type, {string, _, Value}}) ->
    io_lib:format("\"~s\"", [Value]);
generate_expression({literal, Type, {integer, _, Value}}) ->
    io_lib:format("~p", [Value]);
generate_expression({literal, Type, Value}) ->
    case Type of
        string ->
            io_lib:format("\"~s\"", [Value]);
        atom ->
            io_lib:format("~p", [Value]);
        _ ->
            io_lib:format("~p", [Value])
    end;

generate_expression({variable, {var, _, Name}}) ->
    NameStr = atom_to_list(Name),
    string:to_upper(string:slice(NameStr, 0, 1)) ++ string:slice(NameStr, 1);

generate_expression({function_call, {var, _, Name}, Args}) ->
    ArgsStr = generate_expression_list(Args),
    io_lib:format("~s(~s)", [string:to_lower(atom_to_list(Name)), ArgsStr]);

generate_expression({record_create, {var, _, Name}, Fields}) ->
    FieldsStr = generate_record_fields(Fields),
    LowerName = string:to_lower(atom_to_list(Name)),
    io_lib:format("#~s{~s}", [LowerName, FieldsStr]);

generate_expression({record_access, _, Record, {var, _, Field}}) ->
    RecordStr = generate_expression(Record),
    io_lib:format("~s#record.~s", [RecordStr, atom_to_list(Field)]);

generate_expression({list, Elements}) ->
    ElementsStr = generate_expression_list(Elements),
    io_lib:format("[~s]", [ElementsStr]);

generate_expression({tuple, Elements}) ->
    ElementsStr = generate_expression_list(Elements),
    io_lib:format("{~s}", [ElementsStr]);

generate_expression({binary_op, Op, Left, Right}) ->
    LeftStr = generate_expression(Left),
    RightStr = generate_expression(Right),
    io_lib:format("~s ~s ~s", [LeftStr, atom_to_list(Op), RightStr]);

generate_expression({map, _, Elements}) ->
    ElementsStr = generate_map_elements(Elements),
    io_lib:format("#{~s}", [ElementsStr]);

generate_expression({case_expr, _, Expr, Clauses}) ->
    ExprStr = generate_expression(Expr),
    ClausesStr = generate_case_clauses(Clauses),
    io_lib:format("case ~s of\n    ~s\nend", [ExprStr, ClausesStr]);

generate_expression({if_expr, _, Expr, Clauses}) ->
    ExprStr = generate_expression(Expr),
    ClausesStr = generate_if_clauses(Clauses),
    io_lib:format("if ~s ->\n    ~s\nend", [ExprStr, ClausesStr]);

generate_expression({block, _, Expressions}) ->
    ExprStrs = lists:map(fun generate_expression/1, Expressions),
    string:join(ExprStrs, ",\n    ");

generate_expression(Other) ->
    io_lib:format("~p", [Other]).

%% Generate expression lists
generate_expression_list([]) ->
    "";
generate_expression_list(Expressions) ->
    ExprStrs = lists:map(fun generate_expression/1, Expressions),
    string:join(ExprStrs, ", ").

%% Generate patterns
generate_pattern({literal, _, Value}) ->
    io_lib:format("~p", [Value]);
generate_pattern({variable, {var, _, Name}}) ->
    NameStr = atom_to_list(Name),
    string:to_upper(string:slice(NameStr, 0, 1)) ++ string:slice(NameStr, 1);
generate_pattern({wildcard}) ->
    "_";
generate_pattern({list_pattern, Patterns}) ->
    PatternStrs = lists:map(fun generate_pattern/1, Patterns),
    io_lib:format("[~s]", [string:join(PatternStrs, ", ")]);
generate_pattern({tuple_pattern, Patterns}) ->
    PatternStrs = lists:map(fun generate_pattern/1, Patterns),
    io_lib:format("{~s}", [string:join(PatternStrs, ", ")]).

%% Generate record fields
generate_record_fields([]) ->
    "";
generate_record_fields(Fields) ->
    FieldStrs = lists:map(fun generate_record_field_assignment/1, Fields),
    string:join(FieldStrs, ", ").

generate_record_field_assignment({record_field_assignment, {var, _, Name}, Value}) ->
    ValueStr = generate_expression(Value),
    io_lib:format("~s = ~s", [atom_to_list(Name), ValueStr]).

%% Generate map elements
generate_map_elements([]) ->
    "";
generate_map_elements(Elements) ->
    ElementStrs = lists:map(fun generate_map_element/1, Elements),
    string:join(ElementStrs, ", ").

generate_map_element({map_element, Key, Value}) ->
    KeyStr = generate_expression(Key),
    ValueStr = generate_expression(Value),
    io_lib:format("~s => ~s", [KeyStr, ValueStr]).

%% Generate case clauses
generate_case_clauses([]) ->
    "";
generate_case_clauses(Clauses) ->
    ClauseStrs = lists:map(fun generate_case_clause/1, Clauses),
    string:join(ClauseStrs, ";\n    ").

generate_case_clause({case_clause, Pattern, Body}) ->
    PatternStr = generate_pattern(Pattern),
    BodyStr = generate_expression(Body),
    io_lib:format("~s -> ~s", [PatternStr, BodyStr]);
generate_case_clause({case_clause, Pattern, Guard, Body}) ->
    PatternStr = generate_pattern(Pattern),
    GuardStr = generate_expression(Guard),
    BodyStr = generate_expression(Body),
    io_lib:format("~s when ~s -> ~s", [PatternStr, GuardStr, BodyStr]).

%% Generate if clauses
generate_if_clauses([]) ->
    "";
generate_if_clauses(Clauses) ->
    ClauseStrs = lists:map(fun generate_if_clause/1, Clauses),
    string:join(ClauseStrs, ";\n    ").

generate_if_clause({if_clause, Condition, Body}) ->
    ConditionStr = generate_expression(Condition),
    BodyStr = generate_expression(Body),
    io_lib:format("~s -> ~s", [ConditionStr, BodyStr]);
generate_if_clause({else_clause, Body}) ->
    BodyStr = generate_expression(Body),
    io_lib:format("true -> ~s", [BodyStr]).

%% Generate exports
generate_exports(Functions) ->
    PublicFunctions = lists:filtermap(fun is_public_function/1, Functions),
    FunctionNames = lists:map(fun get_function_name/1, PublicFunctions),
    string:join(FunctionNames, ", ").

is_public_function({function_def, Head, Body, public}) ->
    {true, {function_def, Head, Body, public}};
is_public_function(_) ->
    false.

get_function_name({function_def, Head, _, _}) ->
    {function_head, {var, _, Name}, Args} = Head,
    Arity = length(Args),
    io_lib:format("~s/~p", [string:to_lower(atom_to_list(Name)), Arity]).