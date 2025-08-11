module internal

import ast
import lexer

// ========================================
// TYPE EXPRESSION PARSING
// This module handles all type-related parsing, separate from value expressions
// ========================================

// parse_type_expression parses type expressions for type annotations
// This is the main entry point for type parsing
pub fn (mut p LXParser) parse_type_expression() ?ast.TypeExpression {
	return p.parse_union_type()
}

// ========================================
// UNION TYPES
// Grammar: union_type ::= primary_type { '|' primary_type }
// ========================================

// parse_union_type parses union types (type1 | type2 | ...)
fn (mut p LXParser) parse_union_type() ?ast.TypeExpression {
	mut types := []ast.TypeExpression{}
	types << p.parse_primary_type()?

	for p.check(operator_token(.pipe)) {
		p.advance() // consume '|'
		types << p.parse_primary_type()?
	}

	if types.len == 1 {
		return types[0]
	}

	return ast.UnionTypeExpr{
		types:    types
		position: p.get_current_position()
	}
}

// ========================================
// PRIMARY TYPES
// Grammar: primary_type ::= simple_type | parameterized_type | function_type | tuple_type
// ========================================

// parse_primary_type parses primary type expressions
fn (mut p LXParser) parse_primary_type() ?ast.TypeExpression {
	return match p.current {
		lexer.IdentToken, lexer.UpperIdentToken {
			p.parse_named_type()
		}
		lexer.PunctuationToken {
			punct := p.current as lexer.PunctuationToken
			match punct.value {
				.lparen {
					p.parse_function_or_parenthesized_type()
				}
				.lbrace {
					p.parse_tuple_type()
				}
				.lbracket {
					p.parse_list_literal_type()
				}
				else {
					p.add_error('Unexpected punctuation in type expression', 'Got ${p.current.str()}')
					none
				}
			}
		}
		lexer.IntToken {
			p.parse_literal_type()
		}
		lexer.FloatToken {
			p.parse_literal_type()
		}
		lexer.StringToken {
			p.parse_literal_type()
		}
		lexer.AtomToken {
			p.parse_literal_type()
		}
		else {
			p.add_error('Expected type expression', 'Got ${p.current.str()}')
			none
		}
	}
}

// ========================================
// NAMED TYPES
// Grammar: named_type ::= identifier [ '(' type_list ')' ]
// ========================================

// parse_named_type parses named types (simple, parameterized, record types)
fn (mut p LXParser) parse_named_type() ?ast.TypeExpression {
	mut name := p.current.get_value()
	position := p.get_current_position()
	p.advance()

	// Check for qualified type: module.Type or module.record
	if p.check(operator_token(.dot)) {
		p.advance() // consume '.'
		if p.current.is_identifier() {
			type_name := p.current.get_value()
			name = '${name}.${type_name}'
			p.advance()
		} else {
			p.add_error('Expected type name after .', 'Got ${p.current.str()}')
			return none
		}
	}

	// Detect qualified type usage: module.type or module.Record
	if name.contains('.') {
		parts := name.split('.')
		if parts.len == 2 {
			mod := parts[0]
			typ := parts[1]
			// Register usage in the global registry
			if typ.len > 0 && typ[0].is_capital() {
				// Record usage
				if mod != p.module_name {
					if typ !in p.global_registry.record_usages['${mod}.${typ}'] {
						p.global_registry.record_usages['${mod}.${typ}'] << p.module_name
					}
				}
			} else {
				// Type alias usage
				if mod != p.module_name {
					if typ !in p.global_registry.type_usages['${mod}.${typ}'] {
						p.global_registry.type_usages['${mod}.${typ}'] << p.module_name
					}
				}
			}
		}
	}

	// Check for record type: RecordName{field: type, ...}
	if p.check(punctuation_token(.lbrace)) {
		return p.parse_record_type(name, position)
	}

	// Check for parameterized types: list(type), map(key_type, value_type)
	if p.check(punctuation_token(.lparen)) {
		return p.parse_parameterized_type(name, position)
	}

	// Check if it's a type variable (single lowercase letter)
	if name.len == 1 && name[0] >= `a` && name[0] <= `z` {
		return ast.VariableTypeExpr{
			name:     name
			position: position
		}
	}

	// Simple type
	return ast.SimpleTypeExpr{
		name:     name
		position: position
	}
}

// ========================================
// RECORD TYPES
// Grammar: record_type ::= identifier '{' field_type_list '}'
// ========================================

// parse_record_type parses record type definitions
fn (mut p LXParser) parse_record_type(name string, position ast.Position) ?ast.TypeExpression {
	p.advance() // consume '{'

	mut fields := map[string]ast.TypeExpression{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			// Parse field name
			field_name := p.current.get_value()
			if !p.current.is_identifier() {
				p.add_error('Expected field name in record type', 'Got ${p.current.str()}')
				return none
			}
			p.advance()

			// Parse field type annotation
			p.consume(operator_token(.type_cons), 'Expected :: after field name')?

			field_type := p.parse_type_expression()?
			fields[field_name] = field_type

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after record type fields')?

	return ast.RecordTypeExpr{
		name:     name
		fields:   fields
		position: position
	}
}

// ========================================
// PARAMETERIZED TYPES
// Grammar: parameterized_type ::= identifier '(' type_list ')'
// ========================================

// parse_parameterized_type parses parameterized types like list(T), map(K, V)
fn (mut p LXParser) parse_parameterized_type(name string, position ast.Position) ?ast.TypeExpression {
	p.advance() // consume '('

	match name {
		'list' {
			element_type := p.parse_type_expression()?
			p.consume(punctuation_token(.rparen), 'Expected ) after list element type')?
			return ast.ListTypeExpr{
				element_type: element_type
				position:     position
			}
		}
		'map' {
			key_type := p.parse_type_expression()?
			p.consume(punctuation_token(.comma), 'Expected , in map type')?
			value_type := p.parse_type_expression()?
			p.consume(punctuation_token(.rparen), 'Expected ) after map value type')?
			return ast.MapTypeExpr{
				key_type:   key_type
				value_type: value_type
				position:   position
			}
		}
		'tuple' {
			mut element_types := []ast.TypeExpression{}

			if !p.check(punctuation_token(.rparen)) {
				for {
					element_types << p.parse_type_expression()?

					if !p.match(punctuation_token(.comma)) {
						break
					}
				}
			}

			p.consume(punctuation_token(.rparen), 'Expected ) after tuple types')?
			return ast.TupleTypeExpr{
				element_types: element_types
				position:      position
			}
		}
		else {
			// Generic parameterized type
			mut param_types := []ast.TypeExpression{}

			if !p.check(punctuation_token(.rparen)) {
				for {
					param_types << p.parse_type_expression()?

					if !p.match(punctuation_token(.comma)) {
						break
					}
				}
			}

			p.consume(punctuation_token(.rparen), 'Expected ) after type parameters')?
			// For now, return as SimpleTypeExpr since GenericTypeExpr doesn't exist
			return ast.SimpleTypeExpr{
				name:     name + '(' + param_types.map(it.str()).join(', ') + ')'
				position: position
			}
		}
	}
}

// ========================================
// FUNCTION TYPES
// Grammar: function_type ::= '(' param_types ')' '->' return_type
// ========================================

// parse_function_or_parenthesized_type parses function types or parenthesized types
fn (mut p LXParser) parse_function_or_parenthesized_type() ?ast.TypeExpression {
	p.advance() // consume '('

	if p.check(punctuation_token(.rparen)) {
		// Empty parentheses - could be () -> ReturnType
		p.advance() // consume ')'
		if p.check(operator_token(.arrow)) {
			p.advance() // consume '->'
			return_type := p.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: []
				return_type: return_type
				position:    p.get_current_position()
			}
		} else {
			p.add_error('Expected -> after ()', 'Got ${p.current.str()}')
			return none
		}
	}

	// Parse first type
	first_type := p.parse_type_expression()?

	if p.check(punctuation_token(.comma)) {
		// Multiple types - could be function parameters or tuple
		mut types := [first_type]

		for p.match(punctuation_token(.comma)) {
			types << p.parse_type_expression()?
		}

		p.consume(punctuation_token(.rparen), 'Expected ) after type list')?

		// Check if it's a function type
		if p.check(operator_token(.arrow)) {
			p.advance() // consume '->'
			return_type := p.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: types
				return_type: return_type
				position:    p.get_current_position()
			}
		} else {
			// It's a tuple type in parentheses
			return ast.TupleTypeExpr{
				element_types: types
				position:      p.get_current_position()
			}
		}
	} else {
		p.consume(punctuation_token(.rparen), 'Expected ) after type')?

		// Check if it's a function type
		if p.check(operator_token(.arrow)) {
			p.advance() // consume '->'
			return_type := p.parse_type_expression()?
			return ast.FunctionTypeExpr{
				param_types: [first_type]
				return_type: return_type
				position:    p.get_current_position()
			}
		} else {
			// Just a parenthesized type
			return first_type
		}
	}
}

// ========================================
// TUPLE TYPES
// Grammar: tuple_type ::= '{' type_list '}'
// ========================================

// parse_tuple_type parses tuple types {type1, type2, ...}
fn (mut p LXParser) parse_tuple_type() ?ast.TypeExpression {
	position := p.get_current_position()
	p.advance() // consume '{'

	mut element_types := []ast.TypeExpression{}

	if !p.check(punctuation_token(.rbrace)) {
		for {
			element_types << p.parse_type_expression()?

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbrace), 'Expected } after tuple types')?

	return ast.TupleTypeExpr{
		element_types: element_types
		position:      position
	}
}

// ========================================
// LITERAL TYPES
// Grammar: literal_type ::= integer | float | string | atom
// ========================================

// parse_literal_type parses literal types (1, "string", :atom, etc.)
fn (mut p LXParser) parse_literal_type() ?ast.TypeExpression {
	position := p.get_current_position()
	value := p.current.get_value()

	// Check if it's an atom token and preserve the colon
	final_value := match p.current {
		lexer.AtomToken {
			':${value}'
		}
		else {
			value
		}
	}

	p.advance()

	// For now, treat literals as simple types with their value as name
	return ast.SimpleTypeExpr{
		name:     final_value
		position: position
	}
}

// ========================================
// LIST LITERAL TYPES
// Grammar: list_literal_type ::= '[' type_list ']'
// ========================================

// parse_list_literal_type parses list literal types like [1, 2, 3]
fn (mut p LXParser) parse_list_literal_type() ?ast.TypeExpression {
	position := p.get_current_position()
	p.advance() // consume '['

	mut element_types := []ast.TypeExpression{}

	if !p.check(punctuation_token(.rbracket)) {
		for {
			element_types << p.parse_type_expression()?

			if !p.match(punctuation_token(.comma)) {
				break
			}
		}
	}

	p.consume(punctuation_token(.rbracket), 'Expected ] after list literal types')?

	return ast.ListTypeExpr{
		element_type: element_types[0] // For now, use first element type
		position:     position
	}
}
