-module(lx2_ast_meta_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test AST meta information
ast_meta_test() ->
    Source = "x = 42",
    {ok, AST} = lx2:parse(Source),
    [{bind, Meta, _}] = AST,

    % Check meta has line, column, and type
    ?assert(maps:is_key(line, Meta)),
    ?assert(maps:is_key(column, Meta)),
    ?assert(maps:is_key(type, Meta)),

    % Check values
    ?assertEqual(1, maps:get(line, Meta)),
    ?assertEqual(2, maps:get(column, Meta)),
    ?assertEqual(undefined, maps:get(type, Meta)).

%% Test type inference updates meta
type_inference_meta_test() ->
    Source = "x = 42",
    {ok, AST} = lx2:parse(Source),
    {ok, TypedAST, _} = lx2_types:infer_types(AST),
    [{bind, Meta, _}] = TypedAST,

    % Check type was updated
    ?assertEqual(integer, maps:get(type, Meta)).

%% Test macro type checking
macro_type_check_test() ->
    Source = "defmacro +(left :: number, right :: number) do {binary_op, [], [+, left, right]} end",
    {ok, AST} = lx2:parse(Source),
    [{defmacro, Meta, [Name, Args, Body]}] = AST,

    % Check macro definition has correct structure
    ?assertEqual('+', Name),
    ?assertEqual(2, length(Args)).

%% Test macro call with types
macro_call_type_test() ->
    Source = "result = 5 + 3",
    {ok, AST} = lx2:parse(Source),
    {ok, TypedAST, _} = lx2_types:infer_types(AST),
    [{bind, Meta, [_, {macro_call, CallMeta, ['+', Args]}]}] = TypedAST,

    % Check macro call has correct type
    ?assertEqual(number, maps:get(type, CallMeta)),
    ?assertEqual(2, length(Args)).

%% Test multiple statements with semicolon
semicolon_meta_test() ->
    Source = "x = 42; y = \"hello\"",
    {ok, AST} = lx2:parse(Source),
    [Bind1, Bind2] = AST,

    % Check both binds have correct meta
    {bind, Meta1, _} = Bind1,
    {bind, Meta2, _} = Bind2,

    ?assertEqual(1, maps:get(line, Meta1)),
    ?assertEqual(2, maps:get(line, Meta2)).

%% Test macro with complex types
macro_complex_types_test() ->
    Source = "defmacro map(fun :: {fun, [T], U}, list :: {list, T}) do {list_map, [], [fun, list]} end",
    {ok, AST} = lx2:parse(Source),
    [{defmacro, Meta, [Name, Args, Body]}] = AST,

    % Check macro definition
    ?assertEqual('map', Name),
    ?assertEqual(2, length(Args)).

%% Test macro with type variables
macro_type_variables_test() ->
    Source = "defmacro filter(fun :: {fun, [T], boolean}, list :: {list, T}) do {list_filter, [], [fun, list]} end",
    {ok, AST} = lx2:parse(Source),
    [{defmacro, Meta, [Name, Args, Body]}] = AST,

    % Check macro definition
    ?assertEqual('filter', Name),
    ?assertEqual(2, length(Args)).

%% Test macro expansion
macro_expansion_test() ->
    Source = "
        defmacro +(left, right) do
            {binary_op, [], [+, left, right]}
        end

        result = 5 + 3
    ",
    {ok, AST} = lx2:parse(Source),
    {ok, ExpandedAST} = lx2_macros:expand_macros_with_types(AST, lx2_macros:new_macro_env()),

    % Check expansion worked
    ?assert(length(ExpandedAST) > 0).

%% Test type unification
type_unification_test() ->
    Type1 = {type_var, 'T'},
    Type2 = integer,

    case lx2_types:unify_types(Type1, Type2) of
        {ok, Substitution} ->
            ?assertEqual(#{'T' => integer}, Substitution);
        {error, Error} ->
            ?assert(false, "Type unification failed: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test function type unification
function_type_unification_test() ->
    Type1 = {type_fun, [{type_var, 'T'}], {type_var, 'U'}},
    Type2 = {type_fun, [integer], boolean},

    case lx2_types:unify_types(Type1, Type2) of
        {ok, Substitution} ->
            ?assertEqual(#{'T' => integer, 'U' => boolean}, Substitution);
        {error, Error} ->
            ?assert(false, "Function type unification failed: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test list type unification
list_type_unification_test() ->
    Type1 = {type_list, {type_var, 'T'}},
    Type2 = {type_list, integer},

    case lx2_types:unify_types(Type1, Type2) of
        {ok, Substitution} ->
            ?assertEqual(#{'T' => integer}, Substitution);
        {error, Error} ->
            ?assert(false, "List type unification failed: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test tuple type unification
tuple_type_unification_test() ->
    Type1 = {type_tuple, [{type_var, 'T'}, {type_var, 'U'}]},
    Type2 = {type_tuple, [integer, boolean]},

    case lx2_types:unify_types(Type1, Type2) of
        {ok, Substitution} ->
            ?assertEqual(#{'T' => integer, 'U' => boolean}, Substitution);
        {error, Error} ->
            ?assert(false, "Tuple type unification failed: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test macro signature registration
macro_signature_registration_test() ->
    Env = lx2_types:new_type_env(),
    Args = [{'left', integer}, {'right', integer}],
    ReturnType = integer,

    NewEnv = lx2_types:register_macro_signature('+', Args, ReturnType, Env),

    case lx2_types:get_macro_signature('+', 2, NewEnv) of
        {ok, {'+', Args, ReturnType}} ->
            ?assert(true);
        {error, Error} ->
            ?assert(false, "Macro signature registration failed: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test macro signature lookup
macro_signature_lookup_test() ->
    Env = lx2_types:new_type_env(),
    Args = [{'left', integer}, {'right', integer}],
    ReturnType = integer,
    NewEnv = lx2_types:register_macro_signature('+', Args, ReturnType, Env),

    case lx2_types:get_macro_signature('+', 2, NewEnv) of
        {ok, Signature} ->
            ?assertEqual({'+', Args, ReturnType}, Signature);
        {error, Error} ->
            ?assert(false, "Macro signature lookup failed: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test macro signature arity mismatch
macro_signature_arity_mismatch_test() ->
    Env = lx2_types:new_type_env(),
    Args = [{'left', integer}, {'right', integer}],
    ReturnType = integer,
    NewEnv = lx2_types:register_macro_signature('+', Args, ReturnType, Env),

    case lx2_types:get_macro_signature('+', 1, NewEnv) of
        {error, {arity_mismatch, '+', 1}} ->
            ?assert(true);
        {ok, _} ->
            ?assert(false, "Expected arity mismatch error");
        {error, Error} ->
            ?assert(false, "Unexpected error: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test macro not found
macro_not_found_test() ->
    Env = lx2_types:new_type_env(),

    case lx2_types:get_macro_signature('nonexistent', 2, Env) of
        {error, {macro_not_found, 'nonexistent'}} ->
            ?assert(true);
        {ok, _} ->
            ?assert(false, "Expected macro not found error");
        {error, Error} ->
            ?assert(false, "Unexpected error: " ++ lists:flatten(io_lib:format("~p", [Error])))
    end.

%% Test complex macro with type variables
complex_macro_type_test() ->
    Source = "
        defmacro map(fun :: {fun, [T], U}, list :: {list, T}) do
            {list_map, [], [fun, list]}
        end

        defmacro filter(fun :: {fun, [T], boolean}, list :: {list, T}) do
            {list_filter, [], [fun, list]}
        end
    ",
    {ok, AST} = lx2:parse(Source),
    {ok, TypedAST, _} = lx2_types:infer_types(AST),

    % Check both macros were processed
    ?assertEqual(2, length(TypedAST)),

    [Macro1, Macro2] = TypedAST,
    {defmacro, _, [Name1, _, _]} = Macro1,
    {defmacro, _, [Name2, _, _]} = Macro2,

    ?assertEqual('map', Name1),
    ?assertEqual('filter', Name2).

%% Test error handling for invalid macro calls
invalid_macro_call_test() ->
    Source = "result = nonexistent(1, 2, 3)",
    {ok, AST} = lx2:parse(Source),

    case lx2_types:infer_types(AST) of
        {error, _} ->
            ?assert(true);
        {ok, _, _} ->
            ?assert(false, "Expected error for invalid macro call")
    end.

%% Test error handling for type mismatch
type_mismatch_test() ->
    Source = "
        defmacro +(left :: integer, right :: integer) do
            {binary_op, [], [+, left, right]}
        end

        result = 5 + \"hello\"
    ",
    {ok, AST} = lx2:parse(Source),

    case lx2_types:infer_types(AST) of
        {error, _} ->
            ?assert(true);
        {ok, _, _} ->
            ?assert(false, "Expected error for type mismatch")
    end.