module main

fn test_integer_literal() {
	lx_code := 'def int_lit() do
        123
    end'
	expected := '-module(test).
-export([int_lit/0]).

-spec int_lit() -> integer().
int_lit() ->
    123.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_float_literal() {
	lx_code := 'def float_lit() do
        3.14
    end'
	expected := '-module(test).
-export([float_lit/0]).

-spec float_lit() -> float().
float_lit() ->
    3.14.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_string_literal() {
	lx_code := 'def str_lit() do
        "abc"
    end'
	expected := '-module(test).
-export([str_lit/0]).

-spec str_lit() -> binary().
str_lit() ->
    <<"abc"/utf8>>.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_boolean_true_literal() {
	lx_code := 'def bool_true() do
        true
    end'
	expected := '-module(test).
-export([bool_true/0]).

-spec bool_true() -> boolean().
bool_true() ->
    true.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_boolean_false_literal() {
	lx_code := 'def bool_false() do
        false
    end'
	expected := '-module(test).
-export([bool_false/0]).

-spec bool_false() -> boolean().
bool_false() ->
    false.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_atom_literal() {
	lx_code := 'def atom_lit() do
        :ok
    end'
	expected := '-module(test).
-export([atom_lit/0]).

-spec atom_lit() -> atom().
atom_lit() ->
    ok.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_nil_literal() {
	lx_code := 'def nil_lit() do
        nil
    end'
	expected := '-module(test).
-export([nil_lit/0]).

-spec nil_lit() -> nil.
nil_lit() ->
    nil.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_hexadecimal_literal() {
	lx_code := 'def hex_lit() do
        0x1234
    end'
	expected := '-module(test).
-export([hex_lit/0]).

-spec hex_lit() -> integer().
hex_lit() ->
    4660.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_hexadecimal_mixed_case() {
	lx_code := 'def hex_mixed() do
        0xAbCdEf
    end'
	expected := '-module(test).
-export([hex_mixed/0]).

-spec hex_mixed() -> integer().
hex_mixed() ->
    11259375.
'
	result := compile_lx(lx_code)
	assert result == expected
}

fn test_charlist_literal() {
	lx_code := 'def clist() do
        \'hello\'
    end'
	expected := '-module(test).
-export([clist/0]).

-spec clist() -> [integer()].
clist() ->
    "hello".
'
	result := compile_lx(lx_code)
	assert result == expected
}
