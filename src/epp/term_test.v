module epp

fn test_new_atom() {
	assert 'atom' == new_atom('atom').to_string()
}
fn test_new_boolean() {
	assert 'true' == new_boolean(true).to_string()
	assert 'false' == new_boolean(false).to_string()
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
	assert [u8(131),97,1] == term_to_binary(new_integer(1))!
	assert [u8(131),97,255] == term_to_binary(new_integer(255))!
	assert [u8(131),98,0,0,1,0] == term_to_binary(new_integer(256))!
}

fn test_binary_to_term_integer() {
	assert new_integer(1) == binary_to_term([u8(131),97,1])!
	assert new_integer(255) == binary_to_term([u8(131),97,255])!
	assert new_integer(256) == binary_to_term([u8(131),98,0,0,1,0])!
}

fn test_term_to_binary_float() {
	assert [u8(131),70,63,240,0,0,0,0,0,0] == term_to_binary(new_float(1.0))!
}

fn test_binary_to_term_float() {
	assert new_float(1.0001) == binary_to_term([u8(131),70, 63, 240, 0, 104, 219, 139, 172, 113])!
}

fn test_binary_to_term_atom() {
	assert [u8(131), 119, 4, 97, 116, 111, 109] == term_to_binary(new_atom('atom'))!
	// max atom 255ch
	assert [u8(131), 119, 255, 76, 111, 114, 101, 109, 105, 112, 115, 117, 109, 100, 111, 108, 111, 114, 115, 105, 116, 97, 109, 101, 116, 44, 99, 111, 110, 115, 101, 99, 116, 101, 116, 117, 114, 97, 100, 105, 112, 105, 115, 99, 105, 110, 103, 101, 108, 105, 116, 46, 68, 111, 110, 101, 99, 118, 117, 108, 112, 117, 116, 97, 116, 101, 101, 116, 97, 114, 99, 117, 116, 105, 110, 99, 105, 100, 117, 110, 116, 105, 109, 112, 101, 114, 100, 105, 101, 116, 46, 68, 117, 105, 115, 110, 111, 110, 109, 97, 103, 110, 97, 101, 114, 97, 116, 46, 81, 117, 105, 115, 113, 117, 101, 110, 117, 110, 99, 112, 117, 114, 117, 115, 44, 32, 118, 111, 108, 117, 116, 112, 97, 116, 101, 116, 116, 105, 110, 99, 105, 100, 117, 110, 116, 97, 44, 114, 104, 111, 110, 99, 117, 115, 100, 105, 99, 116, 117, 109, 100, 111, 108, 111, 114, 46, 68, 111, 110, 101, 99, 116, 105, 110, 99, 105, 100, 117, 110, 116, 101, 108, 101, 105, 102, 101, 110, 100, 110, 117, 110, 99, 118, 101, 108, 118, 101, 110, 101, 110, 97, 116, 105, 115, 46, 68, 111, 110, 101, 99, 98, 108, 97, 110, 100, 105, 116, 100, 105, 97, 109, 101, 116, 68, 111, 110, 101, 99, 116, 105, 110, 99, 105, 100, 117, 110, 116, 101, 108, 101, 105, 102, 101, 110, 105, 110, 99, 105, 100, 117, 110, 116, 105, 109, 112, 101, 114, 100] == term_to_binary(new_atom('Loremipsumdolorsitamet,consecteturadipiscingelit.Donecvulputateetarcutinciduntimperdiet.Duisnonmagnaerat.Quisquenuncpurus, volutpatettincidunta,rhoncusdictumdolor.Donectincidunteleifendnuncvelvenenatis.DonecblanditdiametDonectincidunteleifeninciduntimperd'))!

	// over max atom 255ch
	expected_error_msg := 'atom length must be less than system: Loremipsumdolorsitamet,consecteturadipiscingelit.Donecvulputateetarcutinciduntimperdiet.Duisnonmagnaerat.Quisquenuncpurus, volutpatettincidunta,rhoncusdictumdolor.Donectincidunteleifendnuncvelvenenatis.DonecblanditdiametDonectincidunteleifeninciduntimperd.'
	mut received_error_msg := ''
	term_to_binary(new_atom('Loremipsumdolorsitamet,consecteturadipiscingelit.Donecvulputateetarcutinciduntimperdiet.Duisnonmagnaerat.Quisquenuncpurus, volutpatettincidunta,rhoncusdictumdolor.Donectincidunteleifendnuncvelvenenatis.DonecblanditdiametDonectincidunteleifeninciduntimperd.')) or {
		received_error_msg = err.msg()
	}
	assert expected_error_msg == received_error_msg
}

fn test_binary_to_term_boolean() {
	assert [u8(131), 119, 4, 116, 114, 117, 101] == term_to_binary(new_boolean(true))!
}