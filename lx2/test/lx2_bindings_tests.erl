-module(lx2_bindings_tests).

-include_lib("eunit/include/eunit.hrl").

%% Task 2: Variables and Local Bindings Tests

%% Test simple variable binding
task2_simple_binding_test() ->
    Source = "def simple_binding() do x = 42; x end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(42, ModuleName:simple_binding()).

%% Test multiple variable bindings
task2_multiple_bindings_test() ->
    Source = "def multiple_bindings() do a = 10; b = 20; a end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(10, ModuleName:multiple_bindings()).

%% Test different types of variables
task2_different_types_test() ->
    Source = "def different_types() do number = 42; text = \"Hello\"; text end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(<<"Hello">>, ModuleName:different_types()).

%% Test explicit separators
task2_explicit_separators_test() ->
    Source = "def explicit_separators() do x = 10; y = 20; x end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(10, ModuleName:explicit_separators()).

%% Test isolated scope
task2_isolated_scope_test() ->
    Source = "def scope1() do x = 42; x end def scope2() do x = 100; x end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(42, ModuleName:scope1()),
    ?assertEqual(100, ModuleName:scope2()).

%% Test variable binding with literals
task2_literal_binding_test() ->
    Source = "def literal_binding() do x = true; y = nil; x end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(true, ModuleName:literal_binding()).

%% Test variable binding with atoms
task2_atom_binding_test() ->
    Source = "def atom_binding() do x = :ok; y = :error; x end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(ok, ModuleName:atom_binding()).

%% Test variable binding with floats
task2_float_binding_test() ->
    Source = "def float_binding() do x = 3.14; y = 2.71; x end",
    {ok, ModuleName, BeamCode, _} = lx2:compile(Source),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(3.14, ModuleName:float_binding()).

%% Test error handling for undefined variables
task2_undefined_variable_test() ->
    Source = "def undefined_var() do x = 42\n y end",
    case lx2:compile(Source) of
        {error, _} ->
            ?assert(true); % Expected error
        _ ->
            ?assert(false) % Should not compile
    end.

%% Test AST generation for variables
task2_ast_generation_test() ->
    Source = "def ast_test() do x = 42\n x end",
    {ok, AST} = lx2:compile(Source, #{mode => ast}),
    ?assert(is_list(AST)),
    ?assert(length(AST) > 0).

%% Test .erl generation for variables
task2_erl_generation_test() ->
    Source = "def erl_test() do x = 42; x end",
    {ok, ErlCode} = lx2:compile(Source, #{mode => erl}),
    ?assert(is_list(ErlCode)),
    ?assert(string:find(ErlCode, "= 42") =/= nomatch).

%% Test both modes (BEAM + .erl)
task2_both_modes_test() ->
    Source = "def both_test() do x = 42; x end",
    {ok, ModuleName, BeamCode, #{source := ErlCode}} = lx2:compile(Source, #{mode => both}),
    {module, ModuleName} = code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(42, ModuleName:both_test()),
    ?assert(is_list(ErlCode)).