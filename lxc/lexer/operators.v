module lexer

// OperatorMap maps operator strings to their corresponding tokens
pub const operator_map = {
	'=':       OperatorToken.assign
	'<-':      OperatorToken.pattern_match
	'->':      OperatorToken.arrow
	'!':       OperatorToken.send
	'::':      OperatorToken.type_cons
	'.':       OperatorToken.dot
	'++':      OperatorToken.concat
	'|':       OperatorToken.record_update
	'+':       OperatorToken.plus
	'-':       OperatorToken.minus
	'*':       OperatorToken.mult
	'/':       OperatorToken.div
	'==':      OperatorToken.eq
	'!=':      OperatorToken.neq
	'<':       OperatorToken.lt
	'>':       OperatorToken.gt
	'<=':      OperatorToken.leq
	'>=':      OperatorToken.geq
	'and':     OperatorToken.and_
	'or':      OperatorToken.or_
	'not':     OperatorToken.not_
	'andalso': OperatorToken.andalso
	'orelse':  OperatorToken.orelse
	'&&':      OperatorToken.andalso
	'||':      OperatorToken.orelse
}

// PunctuationMap maps punctuation strings to their corresponding tokens
pub const punctuation_map = {
	'(': PunctuationToken.lparen
	')': PunctuationToken.rparen
	'{': PunctuationToken.lbrace
	'}': PunctuationToken.rbrace
	'[': PunctuationToken.lbracket
	']': PunctuationToken.rbracket
	',': PunctuationToken.comma
	';': PunctuationToken.semicolon
}

// is_operator checks if a string is an operator
pub fn is_operator(s string) bool {
	return s in operator_map
}

// get_operator_token returns the token for an operator string
pub fn get_operator_token(s string) ?OperatorToken {
	if s in operator_map {
		return operator_map[s]
	}
	return none
}

// is_punctuation checks if a string is punctuation
pub fn is_punctuation(s string) bool {
	return s in punctuation_map
}

// get_punctuation_token returns the token for a punctuation string
pub fn get_punctuation_token(s string) ?PunctuationToken {
	if s in punctuation_map {
		return punctuation_map[s]
	}
	return none
}

// get_all_operators returns all operator strings
pub fn get_all_operators() []string {
	return operator_map.keys()
}

// get_all_punctuation returns all punctuation strings
pub fn get_all_punctuation() []string {
	return punctuation_map.keys()
}

// is_single_char_operator checks if a character can be the start of an operator
pub fn is_single_char_operator(c u8) bool {
	return c == `=` || c == `<` || c == `>` || c == `!` || c == `:` || c == `+` || c == `-`
		|| c == `*` || c == `/` || c == `|` || c == `.` || c == `&`
}

// is_operator_start checks if a character can be the start of an operator
pub fn is_operator_start(c u8) bool {
	return is_single_char_operator(c)
}

// get_operator_precedence returns the precedence of an operator token
pub fn get_operator_precedence(t OperatorToken) int {
	return match t {
		.send { 1 }
		.type_cons { 2 }
		.concat { 3 }
		.plus, .minus { 4 }
		.mult, .div { 5 }
		.eq, .neq, .lt, .gt, .leq, .geq { 6 }
		.and_, .or_, .andalso, .orelse { 7 }
		.not_ { 8 }
		.assign, .pattern_match { 9 }
		else { 10 }
	}
}

// is_left_associative checks if an operator is left associative
pub fn is_left_associative(t OperatorToken) bool {
	return match t {
		.plus, .minus, .mult, .div, .concat, .and_, .or_, .andalso, .orelse { true }
		else { false }
	}
}

// is_right_associative checks if an operator is right associative
pub fn is_right_associative(t OperatorToken) bool {
	return match t {
		.assign, .pattern_match, .send, .type_cons { true }
		else { false }
	}
}
