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

	result := compile_lx(lx_code)
	assert result.contains('greeting() ->')
	assert result.contains('<<"Hello"/utf8>>')
}
