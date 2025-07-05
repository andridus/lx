module internal

import lexer

// Precedence levels for operators
pub enum Precedence {
	none
	assignment // =
	send       // ! (message send)
	or_        // or, orelse
	and_       // and, andalso
	equality   // ==, !=
	comparison // <, >, <=, >=
	term       // +, -
	factor     // *, /
	unary      // -, not
	call       // ., ()
	primary
}

// PrecedenceTable manages operator precedence
pub struct PrecedenceTable {
}

// new_precedence_table creates a new precedence table
pub fn new_precedence_table() PrecedenceTable {
	return PrecedenceTable{}
}

// get_precedence returns the precedence level for an operator token
pub fn (p PrecedenceTable) get_precedence(token lexer.OperatorToken) Precedence {
	return match token.value {
		.assign { .assignment }
		.pattern_match { .assignment }
		.send { .send }
		.or_ { .or_ }
		.and_ { .and_ }
		.eq, .neq { .equality }
		.lt, .gt, .leq, .geq { .comparison }
		.plus, .minus { .term }
		.mult, .div, .modulo { .factor }
		.not_ { .unary }
		.concat { .term }
		.pipe { .term }
		.type_cons { .call }
		.dot { .call }
		.arrow { .none }
		.fat_arrow { .none }
	}
}

// get_precedence_from_token returns precedence from a token
pub fn (p PrecedenceTable) get_precedence_from_token(token lexer.Token) Precedence {
	return match token {
		lexer.OperatorToken { p.get_precedence(token) }
		else { .none }
	}
}

// is_left_associative checks if an operator is left associative
pub fn (p PrecedenceTable) is_left_associative(token lexer.OperatorToken) bool {
	return match token.value {
		.assign { false }
		.pattern_match { false }
		.send { false }
		.or_ { true }
		.and_ { true }
		.eq, .neq, .lt, .gt, .leq, .geq { true }
		.plus, .minus { true }
		.mult, .div, .modulo { true }
		.not_ { false }
		.concat { true }
		.pipe { false }
		.type_cons { false }
		.dot { true }
		.arrow { false }
		.fat_arrow { false }
	}
}

// is_right_associative checks if an operator is right associative
pub fn (p PrecedenceTable) is_right_associative(token lexer.OperatorToken) bool {
	return !p.is_left_associative(token)
}

// compare_precedence compares precedence levels
pub fn (p PrecedenceTable) compare_precedence(left Precedence, right Precedence) int {
	left_val := int(left)
	right_val := int(right)

	if left_val < right_val {
		return -1
	} else if left_val > right_val {
		return 1
	} else {
		return 0
	}
}

// get_operator_precedence is a convenience function for getting precedence
pub fn get_operator_precedence(token lexer.OperatorToken) int {
	table := new_precedence_table()
	return int(table.get_precedence(token))
}

// is_left_associative is a convenience function for checking associativity
pub fn is_left_associative(token lexer.OperatorToken) bool {
	table := new_precedence_table()
	return table.is_left_associative(token)
}

// is_right_associative is a convenience function for checking associativity
pub fn is_right_associative(token lexer.OperatorToken) bool {
	table := new_precedence_table()
	return table.is_right_associative(token)
}
