module utils

// String utilities for LX compiler

// escape_string escapes special characters in a string
pub fn escape_string(s string) string {
	mut result := ''
	for i := 0; i < s.len; i++ {
		ch := s[i]
		match ch {
			`\\` { result += '\\\\' }
			`"` { result += '\\"' }
			`'` { result += "\\'" }
			`\n` { result += '\\n' }
			`\r` { result += '\\r' }
			`\t` { result += '\\t' }
			`\b` { result += '\\b' }
			`\f` { result += '\\f' }
			`\v` { result += '\\v' }
			`\a` { result += '\\a' }
			else {
				if ch < 32 || ch > 126 {
					result += '\\x${ch:02x}'
				} else {
					result += ch.ascii_str()
				}
			}
		}
	}
	return result
}

// unescape_string unescapes a string
pub fn unescape_string(s string) string {
	mut result := ''

	for i := 0; i < s.len; i++ {
		if s[i] == `\\` && i + 1 < s.len {
			next := s[i + 1]
			match next {
				`\\` { result += '\\' }
				`"` { result += '"' }
				`'` { result += "'" }
				`n` { result += '\n' }
				`r` { result += '\r' }
				`t` { result += '\t' }
				`b` { result += '\b' }
				`f` { result += '\f' }
				`v` { result += '\v' }
				`a` { result += '\a' }
				`x` {
					// Handle hex escape sequences
					if i + 3 < s.len {
						hex_str := s[i + 2..i + 4]
						if is_hex_string(hex_str) {
							hex_val := hex_string_to_int(hex_str)
							result += hex_val.str()
							i += 3
						} else {
							result += '\\x'
						}
					} else {
						result += '\\x'
					}
				}
				else { result += '\\${next}' }
			}
			i++ // Skip the escaped character
		} else {
			result += s[i..i+1]
		}
	}

	return result
}

// is_hex_string checks if a string is a valid hex string
fn is_hex_string(s string) bool {
	if s.len != 2 {
		return false
	}
	for ch in s {
		if !((ch >= `0` && ch <= `9`) || (ch >= `a` && ch <= `f`) || (ch >= `A` && ch <= `F`)) {
			return false
		}
	}
	return true
}

// hex_string_to_int converts a hex string to integer
fn hex_string_to_int(s string) int {
	mut result := 0
	for ch in s {
		result *= 16
		if ch >= `0` && ch <= `9` {
			result += int(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			result += int(ch - `a` + 10)
		} else if ch >= `A` && ch <= `F` {
			result += int(ch - `A` + 10)
		}
	}
	return result
}

// is_valid_identifier checks if a string is a valid LX identifier
pub fn is_valid_identifier(s string) bool {
	if s.len == 0 {
		return false
	}

	// First character must be a letter or underscore
	first := s[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}

	// Remaining characters must be letters, digits, or underscores
	for i := 1; i < s.len; i++ {
		ch := s[i]
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}

	return true
}

// is_valid_atom checks if a string is a valid LX atom
pub fn is_valid_atom(s string) bool {
	if s.len == 0 {
		return false
	}

	// Atoms must start with a letter or underscore
	first := s[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}

	// Remaining characters must be letters, digits, or underscores
	for i := 1; i < s.len; i++ {
		ch := s[i]
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}

	return true
}

// is_valid_module_name checks if a string is a valid LX module name
pub fn is_valid_module_name(s string) bool {
	if s.len == 0 {
		return false
	}

	// Module names must start with an uppercase letter
	first := s[0]
	if !(first >= `A` && first <= `Z`) {
		return false
	}

	// Remaining characters must be letters, digits, or underscores
	for i := 1; i < s.len; i++ {
		ch := s[i]
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}

	return true
}

// is_valid_record_name checks if a string is a valid LX record name
pub fn is_valid_record_name(s string) bool {
	// Record names follow the same rules as module names
	return is_valid_module_name(s)
}

// normalize_whitespace normalizes whitespace in a string
pub fn normalize_whitespace(s string) string {
	mut result := ''
	mut in_whitespace := false

	for ch in s {
		if ch.is_space() {
			if !in_whitespace {
				result += ' '
				in_whitespace = true
			}
		} else {
			result += ch.ascii_str()
			in_whitespace = false
		}
	}

	return result.trim(' ')
}

// split_identifier splits a qualified identifier (e.g., "Module.function")
pub fn split_identifier(s string) (string, string) {
	parts := s.split('.')
	if parts.len == 1 {
		return '', parts[0]
	} else if parts.len == 2 {
		return parts[0], parts[1]
	} else {
		// Handle multiple dots by joining all but the last
		mod_name := parts[..parts.len - 1].join('.')
		function := parts[parts.len - 1]
		return mod_name, function
	}
}

// StringPool provides string interning for memory optimization
pub struct StringPool {
mut:
	pool map[string]string
}

// new_string_pool creates a new StringPool
pub fn new_string_pool() StringPool {
	return StringPool{
		pool: map[string]string{}
	}
}

// intern interns a string in the pool
pub fn (mut sp StringPool) intern(s string) string {
	if s in sp.pool {
		return sp.pool[s]
	}
	sp.pool[s] = s
	return s
}

// contains checks if a string is in the pool
pub fn (sp StringPool) contains(s string) bool {
	return s in sp.pool
}

// size returns the number of strings in the pool
pub fn (sp StringPool) size() int {
	return sp.pool.len
}

// clear clears the string pool
pub fn (mut sp StringPool) clear() {
	sp.pool.clear()
}

// get_all returns all strings in the pool
pub fn (sp StringPool) get_all() []string {
	return sp.pool.keys()
}