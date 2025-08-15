module main

fn test_binary_expression_simple() {
	lx_code := '
def simple_binary() do
  <<1, 2, 3>>
end'
	expected := '-module(test).
-export([simple_binary/0]).

-spec simple_binary() -> any().
simple_binary() ->
<<1, 2, 3>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Simple binary expression test passed')
}

fn test_binary_expression_with_variables() {
	lx_code := '
def binary_with_vars() do
  <<x:16, y:32>>
end'
	expected := '-module(test).
-export([binary_with_vars/0]).

-spec binary_with_vars() -> any().
binary_with_vars() ->
<<X:16, Y:32>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Binary expression with variables test passed')
}

fn test_binary_expression_with_qualifiers() {
	lx_code := '
def binary_with_qualifiers() do
  <<value:16/integer>>
end'
	expected := '-module(test).
-export([binary_with_qualifiers/0]).

-spec binary_with_qualifiers() -> any().
binary_with_qualifiers() ->
<<Value:16/integer>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Binary expression with qualifiers test passed')
}

fn test_binary_expression_complex_qualifiers() {
	lx_code := '
def complex_binary() do
  <<data:32/binary-unit:8>>
end'
	expected := '-module(test).
-export([complex_binary/0]).

-spec complex_binary() -> any().
complex_binary() ->
<<Data:32/binary-unit:8>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Complex binary qualifiers test passed')
}

fn test_binary_expression_multiple_qualifiers() {
	lx_code := '
def multi_qualifiers() do
  <<num:8/integer-signed-little>>
end'
	expected := '-module(test).
-export([multi_qualifiers/0]).

-spec multi_qualifiers() -> any().
multi_qualifiers() ->
<<Num:8/integer-signed-little>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Multiple qualifiers test passed')
}

fn test_binary_expression_unit_qualifier() {
	lx_code := '
def with_unit() do
  <<buffer:32/unit:4>>
end'
	expected := '-module(test).
-export([with_unit/0]).

-spec with_unit() -> any().
with_unit() ->
<<Buffer:32/unit:4>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Unit qualifier test passed')
}

fn test_binary_pattern_matching() {
	lx_code := '
def extract_values(data :: binary) do
  <<a:32, b:16, c:16>> = data
  {a, b, c}
end'
	expected := '-module(test).
-export([extract_values/1]).

-spec extract_values(binary()) -> binary | {integer(), integer(), integer()}.
extract_values(Data) when is_binary(Data) ->
<<A:32, B:16, C:16>> = Data,
{A, B, C}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Binary pattern matching test passed')
}

fn test_binary_pattern_matching_with_qualifiers() {
	lx_code := '
def parse_header(packet :: binary) do
  <<version:8/integer, type1:8/integer, length:16/integer-big>> = packet
  {version, type1, length}
end'
	expected := '-module(test).
-export([parse_header/1]).

-spec parse_header(binary()) -> binary | {integer(), integer(), integer()}.
parse_header(Packet) when is_binary(Packet) ->
<<Version:8/integer, Type1:8/integer, Length:16/integer-big>> = Packet,
{Version, Type1, Length}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Binary pattern matching with qualifiers test passed')
}

fn test_binary_mixed_literals_and_variables() {
	lx_code := '
def mixed_binary() do
  <<255, x:16, 0, y:32/integer>>
end'
	expected := '-module(test).
-export([mixed_binary/0]).

-spec mixed_binary() -> any().
mixed_binary() ->
<<255, X:16, 0, Y:32/integer>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Mixed literals and variables test passed')
}

fn test_binary_float_qualifiers() {
	lx_code := '
def float_binary() do
  <<temperature:64/float-big>>
end'
	expected := '-module(test).
-export([float_binary/0]).

-spec float_binary() -> any().
float_binary() ->
<<Temperature:64/float-big>>.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
	println('✓ Float qualifiers test passed')
}
