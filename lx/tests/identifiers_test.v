module main

import frontend.lexer

fn test_variable_identifiers() {
	input := 'x count _unused _123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'count'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == '_unused'

	token4 := lexer0.next_token()
	assert token4 is lexer.IdentToken
	ident_token4 := token4 as lexer.IdentToken
	assert ident_token4.value == '_123'
}

fn test_module_identifiers() {
	input := 'math string list'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'math'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'string'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == 'list'
}

fn test_record_identifiers() {
	input := 'Person UserData Config'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.UpperIdentToken
	upper_ident_token1 := token1 as lexer.UpperIdentToken
	assert upper_ident_token1.value == 'Person'

	token2 := lexer0.next_token()
	assert token2 is lexer.UpperIdentToken
	upper_ident_token2 := token2 as lexer.UpperIdentToken
	assert upper_ident_token2.value == 'UserData'

	token3 := lexer0.next_token()
	assert token3 is lexer.UpperIdentToken
	upper_ident_token3 := token3 as lexer.UpperIdentToken
	assert upper_ident_token3.value == 'Config'
}

fn test_special_identifiers() {
	input := '__MODULE__'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token := lexer0.next_token()
	assert token is lexer.IdentToken
	ident_token := token as lexer.IdentToken
	assert ident_token.value == '__MODULE__'
}

fn test_identifier_with_numbers() {
	input := 'x1 count2 _var123'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x1'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'count2'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == '_var123'
}

fn test_identifier_with_underscores() {
	input := 'user_name first_name _private_var'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'user_name'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'first_name'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == '_private_var'
}

fn test_keyword_not_identifier() {
	input := 'def case if'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.KeywordToken
	keyword_token1 := token1 as lexer.KeywordToken
	assert keyword_token1.value == lexer.KeywordValue.def

	token2 := lexer0.next_token()
	assert token2 is lexer.KeywordToken
	keyword_token2 := token2 as lexer.KeywordToken
	assert keyword_token2.value == lexer.KeywordValue.case_

	token3 := lexer0.next_token()
	assert token3 is lexer.KeywordToken
	keyword_token3 := token3 as lexer.KeywordToken
	assert keyword_token3.value == lexer.KeywordValue.if_
}

fn test_identifier_validation() {
	// Test valid variable identifiers
	assert lexer.is_variable_identifier('x') == true
	assert lexer.is_variable_identifier('count') == true
	assert lexer.is_variable_identifier('_unused') == true
	assert lexer.is_variable_identifier('_123') == true
	assert lexer.is_variable_identifier('user_name') == true
	assert lexer.is_variable_identifier('x1') == true

	// Test invalid variable identifiers
	assert lexer.is_variable_identifier('X') == false // Starts with uppercase
	assert lexer.is_variable_identifier('123') == false // Starts with digit
	assert lexer.is_variable_identifier('') == false // Empty
	assert lexer.is_variable_identifier('def') == false // Keyword
	assert lexer.is_variable_identifier('__MODULE__') == false // Special identifier

	// Test valid module identifiers
	assert lexer.is_module_identifier('math') == true
	assert lexer.is_module_identifier('string') == true
	assert lexer.is_module_identifier('list') == true
	assert lexer.is_module_identifier('user_module') == true

	// Test invalid module identifiers
	assert lexer.is_module_identifier('Math') == false // Starts with uppercase
	assert lexer.is_module_identifier('_module') == false // Starts with underscore
	assert lexer.is_module_identifier('123module') == false // Starts with digit

	// Test valid record identifiers
	assert lexer.is_record_identifier('Person') == true
	assert lexer.is_record_identifier('UserData') == true
	assert lexer.is_record_identifier('Config') == true
	assert lexer.is_record_identifier('User_Record') == true

	// Test invalid record identifiers
	assert lexer.is_record_identifier('person') == false // Starts with lowercase
	assert lexer.is_record_identifier('_Record') == false // Starts with underscore
	assert lexer.is_record_identifier('123Record') == false // Starts with digit

	// Test special identifiers
	assert lexer.is_special_identifier('__MODULE__') == true
	assert lexer.is_special_identifier('__FILE__') == true
	assert lexer.is_special_identifier('__LINE__') == true
	assert lexer.is_special_identifier('__MODULE') == false // Missing trailing __
	assert lexer.is_special_identifier('MODULE__') == false // Missing leading __
	assert lexer.is_special_identifier('__module__') == false // Lowercase
}

fn test_identifier_edge_cases() {
	// Test single character identifiers
	input := 'x y z'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'y'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == 'z'
}

fn test_identifier_with_mixed_case() {
	input := 'userName firstName _privateVar'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'userName'

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token2 := token2 as lexer.IdentToken
	assert ident_token2.value == 'firstName'

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token3 := token3 as lexer.IdentToken
	assert ident_token3.value == '_privateVar'
}

fn test_record_identifier_with_mixed_case() {
	input := 'UserData ConfigData'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.UpperIdentToken
	upper_ident_token1 := token1 as lexer.UpperIdentToken
	assert upper_ident_token1.value == 'UserData'

	token2 := lexer0.next_token()
	assert token2 is lexer.UpperIdentToken
	upper_ident_token2 := token2 as lexer.UpperIdentToken
	assert upper_ident_token2.value == 'ConfigData'
}

fn test_identifier_followed_by_operators() {
	input := 'x = y + z'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token1 := token2 as lexer.OperatorToken
	assert operator_token1.value == lexer.OperatorValue.assign

	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'y'

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token2 := token4 as lexer.OperatorToken
	assert operator_token2.value == lexer.OperatorValue.plus

	token5 := lexer0.next_token()
	assert token5 is lexer.IdentToken
	ident_token3 := token5 as lexer.IdentToken
	assert ident_token3.value == 'z'
}

fn test_identifier_with_keywords() {
	input := 'def x = 42'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.KeywordToken
	keyword_token := token1 as lexer.KeywordToken
	assert keyword_token.value == lexer.KeywordValue.def

	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token := token2 as lexer.IdentToken
	assert ident_token.value == 'x'

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token := token3 as lexer.OperatorToken
	assert operator_token.value == lexer.OperatorValue.assign

	token4 := lexer0.next_token()
	assert token4 is lexer.IntToken
	int_token := token4 as lexer.IntToken
	assert int_token.value == 42
}
