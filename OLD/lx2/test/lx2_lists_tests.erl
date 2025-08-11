-module(lx2_lists_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test list literals
list_literal_empty_test() ->
    Source = "def test() do [] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([], ModuleName:test()).

list_literal_numbers_test() ->
    Source = "def test() do [1, 2, 3] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

list_literal_mixed_test() ->
    Source = "def test() do [1, \"hello\", :ok] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, <<"hello">>, ok], ModuleName:test()).

%% Test list cons
list_cons_simple_test() ->
    Source = "def test() do [1 | [2, 3]] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

list_cons_nested_test() ->
    Source = "def test() do [1 | [2 | [3 | []]]] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

%% Test macro definitions
macro_definition_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        def test() do
            [1, 2] ++ [3, 4]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3, 4], ModuleName:test()).

%% Test list operations with macros
list_concatenation_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        def test() do
            [1, 2] ++ [3, 4]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3, 4], ModuleName:test()).

list_length_test() ->
    Source = "
        defmacro length(list) do
            {length, _, [list]}
        end

        def test() do
            length([1, 2, 3, 4, 5])
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(5, ModuleName:test()).

list_membership_test() ->
    Source = "
        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            3 in [1, 2, 3, 4, 5]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(true, ModuleName:test()).

list_membership_false_test() ->
    Source = "
        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            10 in [1, 2, 3, 4, 5]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(false, ModuleName:test()).

%% Test complex operations with macros
complex_list_operations_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        defmacro length(list) do
            {length, _, [list]}
        end

        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            list1 = [1, 2, 3]
            list2 = [4, 5, 6]
            combined = list1 ++ list2
            size = length(combined)
            has_five = 5 in combined
            {combined, size, has_five}
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual({[1,2,3,4,5,6], 6, true}, ModuleName:test()).

%% Test nested lists with macros
nested_lists_test() ->
    Source = "
        defmacro length(list) do
            {length, _, [list]}
        end

        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            matrix = [[1, 2], [3, 4], [5, 6]]
            first_row = [1, 2]
            has_first = first_row in matrix
            total_elements = length(matrix) * length(first_row)
            {has_first, total_elements}
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual({true, 6}, ModuleName:test()).

%% Test type errors (these should fail compilation)
list_type_error_concatenation_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        def test() do
            [1, 2] ++ 3
        end
    ",
    case lx2:compile(Source) of
        {error, {semantic_error, _}} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

list_type_error_membership_test() ->
    Source = "
        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            3 in 42
        end
    ",
    case lx2:compile(Source) of
        {error, {semantic_error, _}} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

list_type_error_length_test() ->
    Source = "
        defmacro length(list) do
            {length, _, [list]}
        end

        def test() do
            length(42)
        end
    ",
    case lx2:compile(Source) of
        {error, {semantic_error, _}} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

%% Test empty list operations
empty_list_length_test() ->
    Source = "
        defmacro length(list) do
            {length, _, [list]}
        end

        def test() do
            length([])
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(0, ModuleName:test()).

empty_list_membership_test() ->
    Source = "
        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            1 in []
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(false, ModuleName:test()).

%% Test list with variables
list_with_variables_test() ->
    Source = "
        def test() do
            x = 1
            y = 2
            z = 3
            [x, y, z]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

%% Test list cons with variables
list_cons_with_variables_test() ->
    Source = "
        def test() do
            head = 1
            tail = [2, 3, 4]
            [head | tail]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3, 4], ModuleName:test()).