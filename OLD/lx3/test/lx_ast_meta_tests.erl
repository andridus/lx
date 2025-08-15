-module(lx_ast_meta_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/lx.hrl").

%% Test AST meta information
ast_meta_test() ->
    Source = "x = 42",
    {ok, AST} = lx:parse(Source),
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
    {ok, AST} = lx:parse(Source),
    {ok, TypedAST, _} = lx_types:infer_types(AST),
    [{bind, Meta, _}] = TypedAST,

    % Check type was updated
    ?assertEqual(integer, maps:get(type, Meta)).

%% Test macro type checking
macro_type_check_test() ->
    Source = "defmacro +(left :: number, right :: number) do {binary_op, [], [+, left, right]} end",
    {ok, AST} = lx:parse(Source),
    [{defmacro, Meta, [Name, Args, Body]}] = AST,

    % Check macro definition has correct structure
    ?assertEqual('+', Name),
    ?assertEqual(2, length(Args)).

%% Test macro call with types
macro_call_type_test() ->
    Source = "result = 5 + 3",
    {ok, AST} = lx:parse(Source),
    {ok, TypedAST, _} = lx_types:infer_types(AST),
    [{bind, Meta, [_, {macro_call, CallMeta, ['+', Args]}]}] = TypedAST,

    % Check macro call has correct type
    ?assertEqual(any, maps:get(type, CallMeta)),
    ?assertEqual(2, length(Args)).

%% Test multiple statements with semicolon
semicolon_meta_test() ->
    Source = "x = 42; y = \"hello\"",
    {ok, AST} = lx:parse(Source),
    [Bind1, Bind2] = AST,

    % Check both binds have correct meta
    {bind, Meta1, _} = Bind1,
    {bind, Meta2, _} = Bind2,

    ?assertEqual(1, maps:get(line, Meta1)),
    ?assertEqual(2, maps:get(line, Meta2)).

%% Test type annotations
type_annotations_test() ->
    Source = "defmacro map(fun :: {fun, [T], U}, list :: {list, T}) do {list_map, [], [fun, list]} end",
    {ok, AST} = lx:parse(Source),
    [{defmacro, _Meta, [_Name, Args, _Body]}] = AST,

    % Check typed arguments
    ?assertEqual(2, length(Args)),
    [Arg1, Arg2] = Args,
    {typed_ident, _Meta1, [_Name1, Type1]} = Arg1,
    {typed_ident, _Meta2, [_Name2, Type2]} = Arg2,

    % Check type expressions
    {type_fun, _Meta3, [_ArgTypes, _ReturnType]} = Type1,
    {type_list, _Meta4, _ElementType} = Type2.

%% Test basic literals
literals_test() ->
    Source = "x = 42; y = 3.14; z = \"hello\"; w = true",
    {ok, AST} = lx:parse(Source),
    [Bind1, Bind2, Bind3, Bind4] = AST,

    % Check integer
    {bind, _Meta1, [_, {integer, _Meta1a, 42}]} = Bind1,

    % Check float
    {bind, _Meta2, [_, {float, _Meta2a, 3.14}]} = Bind2,

    % Check string
    {bind, _Meta3, [_, {string, _Meta3a, "hello"}]} = Bind3,

    % Check boolean
    {bind, _Meta4, [_, {boolean, _Meta4a, true}]} = Bind4.