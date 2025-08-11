module main

// ============ Binary and Bitstring Tests ============

// Test basic binary literals
fn test_binary_basic() {
	lx_code := 'def test_binary() do
    binary = <<1, 2, 3>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    BINARY_1 = <<1, 2, 3>>,
    BINARY_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary with variables
fn test_binary_variables() {
	lx_code := 'def test_binary() do
    x = 10
    y = 20
    binary = <<x, y>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    X_1 = 10,
    Y_2 = 20,
    BINARY_3 = <<X_1, Y_2>>,
    BINARY_3.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary with size specifications
fn test_binary_with_sizes() {
	lx_code := 'def test_binary() do
    version = 1
    data = 255
    binary = <<version:8, data:16>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    VERSION_1 = 1,
    DATA_2 = 255,
    BINARY_3 = <<VERSION_1:8, DATA_2:16>>,
    BINARY_3.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary with endianness options
fn test_binary_endianness() {
	lx_code := 'def test_binary() do
    value = 0x1234
    big_endian = <<value:16/big>>
    little_endian = <<value:16/little>>
    [big_endian, little_endian]
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> [binary()].
test_binary() ->
    VALUE_1 = 4660,
    BIG_ENDIAN_2 = <<VALUE_1:16/big>>,
    LITTLE_ENDIAN_3 = <<VALUE_1:16/little>>,
    [BIG_ENDIAN_2, LITTLE_ENDIAN_3].
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary with type options
fn test_binary_types() {
	lx_code := 'def test_binary() do
    int_val = 42
    float_val = 3.14
    binary = <<int_val:32/integer, float_val:64/float>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    INT_VAL_1 = 42,
    FLOAT_VAL_2 = 3.14,
    BINARY_3 = <<INT_VAL_1:32/integer, FLOAT_VAL_2:64/float>>,
    BINARY_3.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary with string/binary type
fn test_binary_string() {
	lx_code := 'def test_binary() do
    data = "hello"
    binary = <<data/binary>>
    binary
end'
	expected := '-module(test).
-export([test_binary/0]).

-spec test_binary() -> binary().
test_binary() ->
    DATA_1 = <<"hello"/utf8>>,
    BINARY_2 = <<DATA_1/binary>>,
    BINARY_2.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary pattern matching
fn test_binary_pattern_matching() {
	lx_code := 'def parse_header(binary) do
    <<version:8, size:16, rest/binary>> = binary
    {version, size, rest}
end'
	expected := '-module(test).
-export([parse_header/1]).

-spec parse_header(any()) -> {any(), any(), any()}.
parse_header(BINARY_1) ->
    <<VERSION_2:8, SIZE_3:16, REST_4/binary>> = BINARY_1,
    {VERSION_2, SIZE_3, REST_4}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test complex binary operations
fn test_binary_complex() {
	lx_code := 'def encode_packet(typ, id, payload) do
    payload_size = byte_size(payload)
    <<typ:4, 0:4, id:16/big, payload_size:32/big, payload/binary>>
end

def decode_packet(packet) do
    <<typ:4, _reserved:4, id:16/big, size:32/big, payload:size/binary, _rest/binary>> = packet
    {typ, id, payload}
end'
	expected := '-module(test).
-export([encode_packet/3, decode_packet/1]).

-spec encode_packet(any(), any(), any()) -> binary().
encode_packet(TYP_1, ID_2, PAYLOAD_3) ->
    PAYLOAD_SIZE_4 = byte_size(PAYLOAD_3),
    <<TYP_1:4, 0:4, ID_2:16/big, PAYLOAD_SIZE_4:32/big, PAYLOAD_3/binary>>.
-spec decode_packet(any()) -> {any(), any(), any()}.
decode_packet(PACKET_5) ->
    <<TYP_1:4, _RESERVED_6:4, ID_2:16/big, SIZE_7:32/big, PAYLOAD_3:SIZE_7/binary, _REST_8/binary>> = PACKET_5,
    {TYP_1, ID_2, PAYLOAD_3}.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary comprehensions (if supported)
fn test_binary_list_conversion() {
	lx_code := 'def list_to_binary(list) do
    case list do
        [] -> <<>>
        [h | t] -> <<h, (list_to_binary(t))/binary>>
    end
end'
	expected := '-module(test).
-export([list_to_binary/1]).

-spec list_to_binary(any()) -> binary().
list_to_binary(LIST_1) ->
    case LIST_1 of
        [] ->
            <<>>;
        [H_2 | T_3] ->
            <<H_2, list_to_binary(T_3)/binary>>
    end.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test empty binary
fn test_binary_empty() {
	lx_code := 'def test_empty() do
    empty = <<>>
    empty
end'
	expected := '-module(test).
-export([test_empty/0]).

-spec test_empty() -> binary().
test_empty() ->
    EMPTY_1 = <<>>,
    EMPTY_1.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test binary with mixed options
fn test_binary_mixed_options() {
	lx_code := 'def test_mixed() do
    a = 1
    b = 2
    c = 3.14
    binary = <<a:8/integer-unsigned, b:16/integer-big, c:32/float-little>>
    binary
end'
	expected := '-module(test).
-export([test_mixed/0]).

-spec test_mixed() -> binary().
test_mixed() ->
    A_1 = 1,
    B_2 = 2,
    C_3 = 3.14,
    BINARY_4 = <<A_1:8/integer-unsigned, B_2:16/integer-big, C_3:32/float-little>>,
    BINARY_4.
'
	result := compile_lx(lx_code)
	assert result == expected
}

// Test error cases
fn test_binary_errors() {
	// Missing closing >>
	lx_code := 'def test_function() do
    binary = <<1, 2, 3
    binary
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected ">>" to close binary literal')
}

// Test binary segment errors
fn test_binary_segment_errors() {
	lx_code := 'def test_function() do
    binary = <<1:8/:invalid>>
    binary
end'
	expected := '-module(test).
-export([test_function/0]).

-spec test_function() -> binary().
test_function() ->
    BINARY_1 = <<1:8/:invalid>>,
    BINARY_1.
'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Invalid binary segment option: expected identifier after /')
}
