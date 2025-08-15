-module(ast_structure_test).

-include_lib("eunit/include/eunit.hrl").

%% Test AST structure format
ast_structure_test() ->
    Source = "42",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            ?assert(is_tuple(Node)),
            ?assertEqual(3, tuple_size(Node)),
            {Type, Meta, Args} = Node,
            ?assertEqual(literal, Type),
            ?assert(is_map(Meta)),
            ?assert(maps:is_key(line, Meta)),
            ?assert(maps:is_key(column, Meta)),
            ?assert(maps:is_key(type, Meta));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test AST meta information
ast_meta_test() ->
    Source = "42",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {_Type, Meta, _Args} = Node,
            ?assert(is_integer(maps:get(line, Meta))),
            ?assert(is_integer(maps:get(column, Meta))),
            ?assertEqual(undefined, maps:get(type, Meta));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test string literal AST
string_ast_test() ->
    Source = "\"hello\"",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, Meta, Args} = Node,
            ?assertEqual(literal, Type),
            ?assert(is_map(Meta)),
            ?assertEqual("hello", Args);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test atom AST
atom_ast_test() ->
    Source = ":hello",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, Meta, Args} = Node,
            ?assertEqual(literal, Type),
            ?assert(is_map(Meta)),
            ?assertEqual(hello, Args);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test boolean AST
boolean_ast_test() ->
    Source = "true",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, Meta, Args} = Node,
            ?assertEqual(literal, Type),
            ?assert(is_map(Meta)),
            ?assertEqual(true, Args);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test identifier AST
identifier_ast_test() ->
    Source = "x",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, Meta, Args} = Node,
            ?assertEqual(variable_ref, Type),
            ?assert(is_map(Meta)),
            ?assertEqual(x, Args);
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test variable binding AST
binding_ast_test() ->
    Source = "x = 42",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, Meta, Args} = Node,
            ?assertEqual(variable_binding, Type),
            ?assert(is_map(Meta)),
            ?assertEqual(2, length(Args)),
            [VarName, Expr] = Args,
            ?assertEqual(x, VarName),
            ?assert(is_tuple(Expr));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test multiple statements
multiple_statements_test() ->
    Source = "x = 42; y = \"hello\"",
    case lx2:parse(Source) of
        {ok, AST} ->
            ?assertEqual(2, length(AST)),
            [Bind1, Bind2] = AST,
            {Type1, Meta1, Args1} = Bind1,
            {Type2, Meta2, Args2} = Bind2,
            ?assertEqual(variable_binding, Type1),
            ?assertEqual(variable_binding, Type2),
            ?assert(is_map(Meta1)),
            ?assert(is_map(Meta2));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.

%% Test function definition AST
function_def_test() ->
    Source = "def answer() do 42 end",
    case lx2:parse(Source) of
        {ok, AST} ->
            [Node] = AST,
            {Type, Meta, Args} = Node,
            ?assertEqual(function_def, Type),
            ?assert(is_map(Meta)),
            ?assertEqual(3, length(Args)),
            [Name, Params, Body] = Args,
            ?assertEqual(answer, Name),
            ?assertEqual([], Params),
            ?assert(is_list(Body));
        {error, Error} ->
            io:format("Parse error: ~p~n", [Error]),
            ?assert(false)
    end.