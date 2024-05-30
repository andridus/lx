module epp

fn test_new_atom() {
	assert 'atom' == new_atom('atom').to_string()
}
fn test_new_integer() {
	assert '1' == new_integer(1).to_string()
	assert '1' == new_integer(1).to_string()
	assert '1' == new_integer(1).to_string()
	assert '1' == new_integer(1).to_string()
}
fn test_new_float() {
	assert '1.0' == new_float(1.0).to_string()
}
fn test_new_tuple() {
	assert '{integer,1}' == new_tuple([new_atom('integer'), new_integer(1)]).to_string()
}
fn test_new_list() {
	assert '[integer,1,1.5]' == new_list([new_atom('integer'), new_integer(1), new_float(1.5)]).to_string()
}

fn test_term_to_binary_integer() {
	assert [u8(131),97,1] == term_to_binary(new_integer(1))
	assert [u8(131),97,255] == term_to_binary(new_integer(255))
	assert [u8(131),98,0,0,1,0] == term_to_binary(new_integer(256))
}

fn test_binary_to_term_integer() {
	assert new_integer(1) == binary_to_term([u8(131),97,1])!
	assert new_integer(255) == binary_to_term([u8(131),97,255])!
	assert new_integer(256) == binary_to_term([u8(131),98,0,0,1,0])!
}

fn test_term_to_binary_float() {
	assert [u8(131),70,63,240,0,0,0,0,0,0] == term_to_binary(new_float(1.0))
}

fn test_binary_to_term_float() {
	assert new_float(1.0001) == binary_to_term([u8(131),70, 63, 240, 0, 104, 219, 139, 172, 113])!
}
