module lexer

// OperatorMap maps operator strings to their corresponding values
pub const operator_map = {
	'=':       OperatorValue.assign
	'<-':      OperatorValue.pattern_match
	'->':      OperatorValue.arrow
	'!':       OperatorValue.send
	'::':      OperatorValue.type_cons
	'.':       OperatorValue.dot
	'++':      OperatorValue.concat
	'|':       OperatorValue.pipe
	'+':       OperatorValue.plus
	'-':       OperatorValue.minus
	'*':       OperatorValue.mult
	'/':       OperatorValue.div
	'%':       OperatorValue.modulo
	'==':      OperatorValue.eq
	'!=':      OperatorValue.neq
	'<':       OperatorValue.lt
	'>':       OperatorValue.gt
	'<=':      OperatorValue.leq
	'>=':      OperatorValue.geq
	'and':     OperatorValue.and_
	'or':      OperatorValue.or_
	'not':     OperatorValue.not_
	'andalso': OperatorValue.andalso
	'orelse':  OperatorValue.orelse
	'&&':      OperatorValue.andalso
	'||':      OperatorValue.orelse
}

// PunctuationMap maps punctuation strings to their corresponding values
pub const punctuation_map = {
	'(': PunctuationValue.lparen
	')': PunctuationValue.rparen
	'{': PunctuationValue.lbrace
	'}': PunctuationValue.rbrace
	'[': PunctuationValue.lbracket
	']': PunctuationValue.rbracket
	',': PunctuationValue.comma
	';': PunctuationValue.semicolon
}

// is_operator checks if a string is an operator
pub fn is_operator(s string) bool {
	return s in operator_map
}

// get_operator_token returns the token for an operator string
pub fn get_operator_token(s string, position TokenPosition) ?OperatorToken {
	if s in operator_map {
		return OperatorToken{
			value:    operator_map[s]
			position: position
		}
	}
	return none
}

// is_punctuation checks if a string is punctuation
pub fn is_punctuation(s string) bool {
	return s in punctuation_map
}

// get_punctuation_token returns the token for a punctuation string
pub fn get_punctuation_token(s string, position TokenPosition) ?PunctuationToken {
	if s in punctuation_map {
		return PunctuationToken{
			value:    punctuation_map[s]
			position: position
		}
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
		|| c == `*` || c == `/` || c == `|` || c == `.` || c == `&` || c == `%`
}

// is_operator_start checks if a character can be the start of an operator
pub fn is_operator_start(c u8) bool {
	return is_single_char_operator(c)
}

// get_operator_precedence returns the precedence of an operator token
pub fn get_operator_precedence(t OperatorValue) int {
	return match t {
		.send { 1 }
		.type_cons { 2 }
		.concat { 3 }
		.plus, .minus { 4 }
		.mult, .div, .modulo { 5 }
		.eq, .neq, .lt, .gt, .leq, .geq { 6 }
		.and_, .or_, .andalso, .orelse { 7 }
		.not_ { 8 }
		.assign, .pattern_match { 9 }
		else { 10 }
	}
}

// is_left_associative checks if an operator is left associative
pub fn is_left_associative(t OperatorValue) bool {
	return match t {
		.plus, .minus, .mult, .div, .modulo, .concat, .and_, .or_, .andalso, .orelse { true }
		else { false }
	}
}

// is_right_associative checks if an operator is right associative
pub fn is_right_associative(t OperatorValue) bool {
	return match t {
		.assign, .pattern_match, .send, .type_cons { true }
		else { false }
	}
}
