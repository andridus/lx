-module(lx2_basic_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test basic lexer functionality
lexer_basic_test() ->
    Source = "def answer() do 42 end",
    {ok, Tokens, _} = lx2_lexer:string(Source),
    ?assertMatch([
        {def, 1},
        {identifier, 1, answer},
        {'(', 1},
        {')', 1},
        {do, 1},
        {integer, 1, 42},
        {'end', 1}
    ], Tokens).

%% Test basic parser functionality
parser_basic_test() ->
    Source = "def answer() do 42 end",
    {ok, Tokens, _} = lx2_lexer:string(Source),
    {ok, AST} = lx2_parser:parse(Tokens),
    ?assertMatch([
        {function_def, answer, [], [{literal, integer, 42}]}
    ], AST).

%% Test basic compilation
compilation_basic_test() ->
    Source = "def answer() do 42 end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    ?assertEqual(unamed, ModuleName),
    ?assert(is_binary(BeamCode)).

%% Test function execution
function_execution_test() ->
    Source = "def answer() do 42 end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),

    % Load the module
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),

    % Execute the function
    Result = ModuleName:answer(),
    ?assertEqual(42, Result).

%% Test different literal types
literals_test() ->
    Source = "def test() do 42 end\ndef test2() do 3.14 end\ndef test3() do \"hello\" end\ndef test4() do :ok end\ndef test5() do true end\ndef test6() do nil end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),

    % Load the module
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),

    % Test each function
    ?assertEqual(42, ModuleName:test()),
    ?assertEqual(3.14, ModuleName:test2()),
    ?assertEqual(<<"hello">>, ModuleName:test3()),
    ?assertEqual(ok, ModuleName:test4()),
    ?assertEqual(true, ModuleName:test5()),
    ?assertEqual(nil, ModuleName:test6()).

%% Test multiple functions
multiple_functions_test() ->
    Source = "def one() do 1 end\ndef two() do 2 end\ndef three() do 3 end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),

    % Load the module
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),

    % Test each function
    ?assertEqual(1, ModuleName:one()),
    ?assertEqual(2, ModuleName:two()),
    ?assertEqual(3, ModuleName:three()).

%% Test .erl generation
erl_generation_test() ->
    Source = "def answer() do 42 end",
    {ok, ErlCode} = lx2:compile(Source, #{mode => erl}),

    % Check if generated code contains expected elements
    ?assert(is_list(ErlCode)),
    ?assert(string:str(ErlCode, "-module(") > 0),
    ?assert(string:str(ErlCode, "-export(") > 0),
    ?assert(string:str(ErlCode, "answer() ->") > 0),
    ?assert(string:str(ErlCode, "42") > 0).

%% Test both modes
both_modes_test() ->
    Source = "def answer() do 42 end",
    {ok, _ModuleName, BeamCode, ErlCode, _DebugInfo} = lx2:compile(Source, #{mode => both}),

    % Check BEAM compilation
    ?assert(is_binary(BeamCode)),

    % Check .erl generation
    ?assert(is_list(ErlCode)),
    ?assert(string:str(ErlCode, "-module(") > 0).