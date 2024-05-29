module epp

fn test_new_atom() {
	assert 'atom' == new_atom('atom').to_string()
}
fn test_new_integer() {
	assert '1' == new_integer(1, .int32).to_string()
	assert '1' == new_integer(1, .uint8).to_string()
	assert '1' == new_integer(1, .big).to_string()
}
fn test_new_float() {
	assert '1.0' == new_float(1.0).to_string()
}
fn test_new_tuple() {
	assert '{integer,1}' == new_tuple([new_atom('integer'), new_integer(1, .int32)]).to_string()
}
fn test_new_list() {
	assert '[integer,1,1.5]' == new_list([new_atom('integer'), new_integer(1, .int32), new_float(1.5)]).to_string()
}
