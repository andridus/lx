module lexer

// KeywordMap maps keyword strings to their corresponding tokens
pub const keyword_map = {
	'def':          KeywordToken.def
	'defp':         KeywordToken.defp
	'case':         KeywordToken.case_
	'if':           KeywordToken.if_
	'else':         KeywordToken.else_
	'do':           KeywordToken.do_
	'end':          KeywordToken.end_
	'with':         KeywordToken.with
	'for':          KeywordToken.for_
	'when':         KeywordToken.when
	'receive':      KeywordToken.receive
	'after':        KeywordToken.after
	'true':         KeywordToken.true_
	'false':        KeywordToken.false_
	'nil':          KeywordToken.nil_
	'unsafe':       KeywordToken.unsafe
	'record':       KeywordToken.record
	'worker':       KeywordToken.worker
	'supervisor':   KeywordToken.supervisor
	'strategy':     KeywordToken.strategy
	'children':     KeywordToken.children
	'one_for_one':  KeywordToken.one_for_one
	'one_for_all':  KeywordToken.one_for_all
	'rest_for_one': KeywordToken.rest_for_one
	'spec':         KeywordToken.spec
	'requires':     KeywordToken.requires
	'ensures':      KeywordToken.ensures
	'matches':      KeywordToken.matches
	'describe':     KeywordToken.describe
	'test':         KeywordToken.test_
	'assert':       KeywordToken.assert
}

// is_keyword checks if a string is a keyword
pub fn is_keyword(s string) bool {
	return s in keyword_map
}

// get_keyword_token returns the token for a keyword string
pub fn get_keyword_token(s string) ?KeywordToken {
	if s in keyword_map {
		return keyword_map[s]
	}
	return none
}

// get_all_keywords returns all keyword strings
pub fn get_all_keywords() []string {
	return keyword_map.keys()
}

// is_reserved checks if a string is reserved (keyword or special identifier)
pub fn is_reserved(s string) bool {
	if is_keyword(s) {
		return true
	}
	// Check for special identifiers like __MODULE__
	return s.starts_with('__') && s.ends_with('__')
}

// validate_identifier checks if an identifier name is valid
pub fn validate_identifier(s string) bool {
	if s.len == 0 {
		return false
	}

	// Check if it's a reserved word
	if is_reserved(s) {
		return false
	}

	// Check first character
	first := s[0]
	if !first.is_letter() && first != `_` {
		return false
	}

	// Check remaining characters
	for i := 1; i < s.len; i++ {
		c := s[i]
		if !c.is_letter() && !c.is_digit() && c != `_` {
			return false
		}
	}

	return true
}

// is_variable_identifier checks if an identifier is a valid variable name
pub fn is_variable_identifier(s string) bool {
	if s.len == 0 {
		return false
	}

	// Variables must start with lowercase letter or underscore
	first := s[0]
	if first.is_capital() && first != `_` {
		return false
	}

	return validate_identifier(s)
}

// is_module_identifier checks if an identifier is a valid module name
pub fn is_module_identifier(s string) bool {
	if s.len == 0 {
		return false
	}

	// Modules must start with lowercase letter (not underscore or digit)
	first := s[0]
	if first < `a` || first > `z` {
		return false
	}

	return validate_identifier(s)
}

// is_record_identifier checks if an identifier is a valid record name
pub fn is_record_identifier(s string) bool {
	if s.len == 0 {
		return false
	}

	// Records must start with uppercase letter
	first := s[0]
	if !first.is_capital() {
		return false
	}

	return validate_identifier(s)
}

// is_special_identifier checks if an identifier is a special identifier
pub fn is_special_identifier(s string) bool {
	if !(s.starts_with('__') && s.ends_with('__')) {
		return false
	}
	inner := s[2..s.len - 2]
	for c in inner {
		if !(c.is_capital() || c == `_`) {
			return false
		}
	}
	return true
}
