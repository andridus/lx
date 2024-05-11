module gen

fn test_generate_erlang_sum_two_integer() {
	source := '1 + 2'
	expected := '
	-module(script).
	-export([run/0]).

	run() -> 1 + 2.\n'
	assert expected == generate_erlang(source)!
}
