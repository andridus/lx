module main

// ============ Binary and Bitstring Tests ============

// Test basic binary literals
fn test_binary_basic() {
	lx_code := 'def test_binary() do
    binary = <<1, 2, 3>>
    binary
end'
	result := compile_lx(lx_code)
	assert result.contains('-module(test)')
	assert result.contains('<<1, 2, 3>>')
	assert result.contains('test_binary')
}

// Test binary with variables
fn test_binary_variables() {
	lx_code := 'def test_binary() do
    x = 10
    y = 20
    binary = <<x, y>>
    binary
end'
	result := compile_lx(lx_code)
	assert result.contains('-module(test)')
	assert result.contains('<<') && result.contains('>>')
}

// Test binary with size specifications
fn test_binary_with_sizes() {
	lx_code := 'def test_binary() do
    version = 1
    data = 255
    binary = <<version:8, data:16>>
    binary
end'
	result := compile_lx(lx_code)
	assert result.contains(':8') && result.contains(':16')
}

// Test binary with endianness options
fn test_binary_endianness() {
	lx_code := 'def test_binary() do
    value = 0x1234
    big_endian = <<value:16/big>>
    little_endian = <<value:16/little>>
    [big_endian, little_endian]
end'
	result := compile_lx(lx_code)
	assert result.contains('<<VALUE_2:16/big>>')
	assert result.contains('<<VALUE_2:16/little>>')
}

// Test binary with type options
fn test_binary_types() {
	lx_code := 'def test_binary() do
    int_val = 42
    float_val = 3.14
    binary = <<int_val:32/integer, float_val:64/float>>
    binary
end'
	result := compile_lx(lx_code)
	assert result.contains('<<INT_VAL_2:32/integer, FLOAT_VAL_3:64/float>>')
}

// Test binary with string/binary type
fn test_binary_string() {
	lx_code := 'def test_binary() do
    data = "hello"
    binary = <<data/binary>>
    binary
end'
	result := compile_lx(lx_code)
	assert result.contains('<<DATA_2/binary>>')
}

// Test binary pattern matching
fn test_binary_pattern_matching() {
	lx_code := 'def parse_header(binary) do
    <<version:8, size:16, rest/binary>> = binary
    {version, size, rest}
end'
	result := compile_lx(lx_code)
	assert result.contains('<<VERSION_2:8, SIZE_3:16, REST_4/binary>>')
	assert result.contains('{VERSION_2, SIZE_3, REST_4}')
}

// Test complex binary operations
fn test_binary_complex() {
	lx_code := 'def encode_packet(type, id, payload) do
    payload_size = byte_size(payload)
    <<type:4, 0:4, id:16/big, payload_size:32/big, payload/binary>>
end

def decode_packet(packet) do
    <<type:4, _reserved:4, id:16/big, size:32/big, payload:size/binary, _rest/binary>> = packet
    {type, id, payload}
end'
	result := compile_lx(lx_code)
	assert result.contains('encode_packet')
	assert result.contains('decode_packet')
	assert result.contains('<<TYPE_4:4, 0:4')
	assert result.contains('SIZE_6:32/big')
}

// Test binary comprehensions (if supported)
fn test_binary_list_conversion() {
	lx_code := 'def list_to_binary(list) do
    case list do
        [] -> <<>>
        [h | t] -> <<h, (list_to_binary(t))/binary>>
    end
end'
	result := compile_lx(lx_code)
	assert result.contains('<<H_3, (list_to_binary(T_4))/binary>>')
}

// Test empty binary
fn test_binary_empty() {
	lx_code := 'def test_empty() do
    empty = <<>>
    empty
end'
	result := compile_lx(lx_code)
	assert result.contains('<<>>')
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
	result := compile_lx(lx_code)
	assert result.contains('<<A_2:8/integer-unsigned')
	assert result.contains('B_3:16/integer-big')
	assert result.contains('C_4:32/float-little>>')
}

// Test error cases
fn test_binary_errors() {
	// Missing closing >>
	lx_code := 'def test() do
    binary = <<1, 2, 3
    binary
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('Expected ">>" to close binary literal') || result.contains('falha') || result.contains('error')
}

// Test binary segment errors
fn test_binary_segment_errors() {
	lx_code := 'def test() do
    binary = <<1:8/:invalid>>
    binary
end'
	result := compile_lx_with_error(lx_code)
	assert result.contains('falha') || result.contains('error')
}