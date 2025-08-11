module main

fn test_simple_list_comprehension() {
	lx_code := 'def simple_comprehension() do
    numbers = [1, 2, 3, 4, 5]
    for x in numbers do
        x
    end
end'

	expected := '-module(test).
-export([simple_comprehension/0]).

-spec simple_comprehension() -> [integer()].
simple_comprehension() ->
    NUMBERS_1 = [1, 2, 3, 4, 5],
    [X_2 || X_2 <- NUMBERS_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_with_condition() {
	lx_code := 'def filtered_comprehension() do
    numbers = [1, 2, 3, 4, 5]
    for x in numbers when x > 2 do
        x
    end
end'

	expected := '-module(test).
-export([filtered_comprehension/0]).

-spec filtered_comprehension() -> [integer()].
filtered_comprehension() ->
    NUMBERS_1 = [1, 2, 3, 4, 5],
    [X_2 || X_2 <- NUMBERS_1, X_2 > 2].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_with_transformation() {
	lx_code := 'def squares() do
    numbers = [1, 2, 3, 4]
    for x in numbers do
        *(x, x)
    end
end'

	expected := '-module(test).
-export([squares/0]).

-spec squares() -> [integer()].
squares() ->
    NUMBERS_1 = [1, 2, 3, 4],
    [X_2 * X_2 || X_2 <- NUMBERS_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_with_condition_and_transformation() {
	lx_code := 'def even_squares() do
    numbers = [1, 2, 3, 4, 5, 6]
    for x in numbers when ==(*(x, 2), 0) do
        *(x, x)
    end
end'

	expected := '-module(test).
-export([even_squares/0]).

-spec even_squares() -> [integer()].
even_squares() ->
    NUMBERS_1 = [1, 2, 3, 4, 5, 6],
    [X_2 * X_2 || X_2 <- NUMBERS_1, X_2 * 2 == 0].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_nested_lists() {
	lx_code := 'def nested_comprehension() do
    matrix = [[1, 2], [3, 4], [5, 6]]
    for row in matrix do
        for x in row do
            +(x, 1)
        end
    end
end'

	expected := '-module(test).
-export([nested_comprehension/0]).

-spec nested_comprehension() -> [[integer()]].
nested_comprehension() ->
    MATRIX_1 = [[1, 2], [3, 4], [5, 6]],
    [[X_2 + 1 || X_2 <- ROW_3] || ROW_3 <- MATRIX_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_string_list() {
	lx_code := 'def string_lengths() do
    words = ["hello", "world", "lx"]
    for word in words do
        length(word)
    end
end'

	expected := '-module(test).
-export([string_lengths/0]).

-spec string_lengths() -> [integer()].
string_lengths() ->
    WORDS_1 = [<<"hello"/utf8>>, <<"world"/utf8>>, <<"lx"/utf8>>],
    [length(WORD_2) || WORD_2 <- WORDS_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_with_multiple_conditions() {
	lx_code := 'def complex_filter() do
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    for x in numbers when and(>(x, 3), <(x, 8)) do
        *(x, 2)
    end
end'

	expected := '-module(test).
-export([complex_filter/0]).

-spec complex_filter() -> [integer()].
complex_filter() ->
    NUMBERS_1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    [X_2 * 2 || X_2 <- NUMBERS_1, X_2 > 3 andalso X_2 < 8].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_tuple_transformation() {
	lx_code := 'def tuple_pairs() do
    numbers = [1, 2, 3]
    for x in numbers do
        {x, *(x, x)}
    end
end'

	expected := '-module(test).
-export([tuple_pairs/0]).

-spec tuple_pairs() -> [{integer(), integer()}].
tuple_pairs() ->
    NUMBERS_1 = [1, 2, 3],
    [{X_2, X_2 * X_2} || X_2 <- NUMBERS_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_empty_result() {
	lx_code := 'def empty_filter() do
    numbers = [1, 2, 3]
    for x in numbers when >(x, 10) do
        x
    end
end'

	expected := '-module(test).
-export([empty_filter/0]).

-spec empty_filter() -> [integer()].
empty_filter() ->
    NUMBERS_1 = [1, 2, 3],
    [X_2 || X_2 <- NUMBERS_1, X_2 > 10].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_with_membership() {
	lx_code := 'def membership_filter() do
    numbers = [1, 2, 3, 4, 5]
    allowed = [2, 4]
    for x in numbers when x in allowed do
        *(x, 10)
    end
end'

	expected := '-module(test).
-export([membership_filter/0]).

-spec membership_filter() -> [integer()].
membership_filter() ->
    NUMBERS_1 = [1, 2, 3, 4, 5],
    ALLOWED_2 = [2, 4],
    [X_3 * 10 || X_3 <- NUMBERS_1, lists:member(X_3, ALLOWED_2)].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_atom_filter() {
	lx_code := 'def atom_comprehension() do
    statuses = [:ok, :error, :pending, :ok, :error]
    for status in statuses when status == :ok do
        :success
    end
end'

	expected := '-module(test).
-export([atom_comprehension/0]).

-spec atom_comprehension() -> [atom()].
atom_comprehension() ->
    STATUSES_1 = [ok, error, pending, ok, error],
    [success || STATUS_2 <- STATUSES_1, STATUS_2 == ok].
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_list_comprehension_mixed_types() {
	lx_code := 'def mixed_transformation() do
    items = [1, "hello", 3.14, :atom]
    for item in items do
        {item, "value"}
    end
end'

	expected := '-module(test).
-export([mixed_transformation/0]).

-spec mixed_transformation() -> [{integer() | binary() | float() | atom(), binary()}].
mixed_transformation() ->
    ITEMS_1 = [1, <<"hello"/utf8>>, 3.14, atom],
    [{ITEM_2, <<"value"/utf8>>} || ITEM_2 <- ITEMS_1].
'

	result := compile_lx(lx_code)
	assert result == expected
}