module parser

import lexer

// Precedence levels for operators
pub enum Precedence {
	none
	assignment // =
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
	return match token {
		.assign { .assignment }
		.pattern_match { .assignment }
		.or_ { .or_ }
		.orelse { .or_ }
		.and_ { .and_ }
		.andalso { .and_ }
		.eq { .equality }
		.neq { .equality }
		.lt { .comparison }
		.gt { .comparison }
		.leq { .comparison }
		.geq { .comparison }
		.plus { .term }
		.minus { .term }
		.mult { .factor }
		.div { .factor }
		.not_ { .unary }
		.dot { .call }
		.arrow { .none }
		.send { .none }
		.type_cons { .none }
		.concat { .none }
		.record_update { .none }
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
	return match token {
		.assign { false }
		.pattern_match { false }
		.or_ { true }
		.orelse { true }
		.and_ { true }
		.andalso { true }
		.eq { false }
		.neq { false }
		.lt { false }
		.gt { false }
		.leq { false }
		.geq { false }
		.plus { true }
		.minus { true }
		.mult { true }
		.div { true }
		.not_ { false }
		.dot { true }
		.arrow { false }
		.send { false }
		.type_cons { false }
		.concat { true }
		.record_update { false }
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
