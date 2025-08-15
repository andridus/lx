module main

fn test_simple_integer() {
	lx_code := 'def answer() do
    42
end'

	expected := '-module(test).
-export([answer/0]).

-spec answer() -> integer().
answer() ->
    42.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_simple_string() {
	lx_code := 'def greeting() do
    "Hello"
end'

	expected := '-module(test).
-export([greeting/0]).

-spec greeting() -> binary().
greeting() ->
    <<"Hello"/utf8>>.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_pipeline_and_capture() {
	lx_code := 'def apply_all(xs) do
        xs
        |> lists:map(&double/1, _)
        |> lists:filter(&is_even/1, _)
    end

    def double(x) do
        x * 2
    end

    def is_even(x) do
        erlang:rem(x, 2) == 0
    end'

	expected := '-module(test).
-export([apply_all/1, double/1, is_even/1]).

-spec apply_all(any()) -> any().
apply_all(XS_1) ->
    lists:filter(fun(ARG1_2) -> is_even(ARG1_2) end, lists:map(fun(ARG1_2) -> double(ARG1_2) end, XS_1)).
-spec double(any()) -> integer().
double(X_3) ->
    X_3 * 2.
-spec is_even(any()) -> boolean().
is_even(X_3) ->
    erlang:rem(X_3, 2) == 0.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_pipeline_default_first_arg() {
	lx_code := "def join_ext(path) do
        path
        |> filename:join('*.beam')
    end"

	expected := '-module(test).
-export([join_ext/1]).

-spec join_ext(any()) -> any().
join_ext(PATH_1) ->
    filename:join(PATH_1, "*.beam").
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_function_capture_module() {
	lx_code := 'def to_atom(s) do
        erlang:apply(:erlang, :list_to_atom, [s])
    end'

	expected := '-module(test).
-export([to_atom/1]).

-spec to_atom(any()) -> any().
to_atom(S_1) ->
    erlang:apply(erlang, list_to_atom, [S_1]).
'

	result := compile_lx(lx_code)
	assert result == expected
}
