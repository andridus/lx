module analysis1

import ast

// TypeInfo representa um tipo com base genérica e valor específico (quando possível)
pub struct TypeInfo {
pub mut:
	generic string  // 'atom', 'integer', 'string', etc.
	value   ?string // ':ok', '1', etc. ou none
}

// Convert TypeInfo to string representation
pub fn (ti TypeInfo) str() string {
	if value := ti.value {
		return '${ti.generic}(${value})'
	}
	return ti.generic
}

// Cria um TypeInfo a partir de uma string de tipo usando a estrutura correta dos tipos nativos
pub fn typeinfo_from_str(type_expr string) TypeInfo {
	// Tipos básicos nativos (sempre genéricos)
	if type_expr == 'integer' || type_expr == 'integer()' {
		return TypeInfo{
			generic: 'integer'
			value:   none
		}
	}
	if type_expr == 'float' || type_expr == 'float()' {
		return TypeInfo{
			generic: 'float'
			value:   none
		}
	}
	if type_expr == 'string' || type_expr == 'string()' {
		return TypeInfo{
			generic: 'string'
			value:   none
		}
	}
	if type_expr == 'boolean' || type_expr == 'boolean()' {
		return TypeInfo{
			generic: 'boolean'
			value:   none
		}
	}
	if type_expr == 'atom' || type_expr == 'atom()' {
		return TypeInfo{
			generic: 'atom'
			value:   none
		}
	}
	if type_expr == 'nil' {
		return TypeInfo{
			generic: 'nil'
			value:   none
		}
	}
	if type_expr == 'any' || type_expr == 'any()' {
		return TypeInfo{
			generic: 'any'
			value:   none
		}
	}
	if type_expr == 'pid' || type_expr == 'pid()' {
		return TypeInfo{
			generic: 'pid'
			value:   none
		}
	}
	if type_expr == 'port' || type_expr == 'port()' {
		return TypeInfo{
			generic: 'port'
			value:   none
		}
	}
	if type_expr == 'reference' || type_expr == 'reference()' {
		return TypeInfo{
			generic: 'reference'
			value:   none
		}
	}
	if type_expr == 'bitstring' || type_expr == 'bitstring()' {
		return TypeInfo{
			generic: 'bitstring'
			value:   none
		}
	}
	if type_expr == 'fun' || type_expr == 'fun()' {
		return TypeInfo{
			generic: 'fun'
			value:   none
		}
	}
	if type_expr == 'list' || type_expr == 'list()' {
		return TypeInfo{
			generic: 'list'
			value:   none
		}
	}
	if type_expr == 'map' || type_expr == 'map()' {
		return TypeInfo{
			generic: 'map'
			value:   none
		}
	}
	if type_expr == 'tuple' || type_expr == 'tuple()' {
		return TypeInfo{
			generic: 'tuple'
			value:   none
		}
	}

	// Detectar valores específicos dinamicamente
	// Átomos específicos (começam com : ou são strings sem aspas)
	if type_expr.starts_with(':') {
		// Átomo específico (ex: :ok, :error)
		atom_value := type_expr[1..]
		return TypeInfo{
			generic: 'atom'
			value:   atom_value
		}
	}

	// Booleanos específicos
	if type_expr == 'true' || type_expr == 'false' {
		return TypeInfo{
			generic: 'boolean'
			value:   type_expr
		}
	}

	// Inteiros específicos (números sem ponto)
	if is_integer_literal(type_expr) {
		return TypeInfo{
			generic: 'integer'
			value:   type_expr
		}
	}

	// Floats específicos (números com ponto)
	if is_float_literal(type_expr) {
		return TypeInfo{
			generic: 'float'
			value:   type_expr
		}
	}

	// Records (começam com #)
	if type_expr.starts_with('#') {
		return TypeInfo{
			generic: 'record'
			value:   type_expr
		}
	}

	// Tuplas (começam com {)
	if type_expr.starts_with('{') {
		return typeinfo_from_tuple(type_expr)
	}

	// Lists com tipo específico (ex: list(integer))
	if type_expr.starts_with('list(') {
		return TypeInfo{
			generic: 'list'
			value:   type_expr
		}
	}

	// Maps com tipos específicos (ex: map(integer=>string))
	if type_expr.starts_with('map(') {
		return TypeInfo{
			generic: 'map'
			value:   type_expr
		}
	}

	// Tuples com tipos específicos (ex: tuple(integer, float))
	if type_expr.starts_with('tuple(') {
		return TypeInfo{
			generic: 'tuple'
			value:   type_expr
		}
	}

	// Union types (contêm |)
	if type_expr.contains('|') {
		return TypeInfo{
			generic: 'union'
			value:   type_expr
		}
	}

	// User defined types (não são built-in) - começam com maiúscula
	if type_expr.len > 0 && type_expr[0].is_capital() {
		return TypeInfo{
			generic: 'record'
			value:   type_expr
		}
	}

	// Átomos genéricos (lowercase identifiers que não são tipos built-in)
	if type_expr.len > 0 && type_expr[0] >= `a` && type_expr[0] <= `z` {
		return TypeInfo{
			generic: 'atom'
			value:   type_expr
		}
	}

	// Default case - unknown type
	return TypeInfo{
		generic: 'unknown'
		value:   none
	}
}

// Helper functions for literal detection
fn is_integer_literal(s string) bool {
	if s.len == 0 {
		return false
	}
	for c in s {
		if !c.is_digit() {
			return false
		}
	}
	return true
}

fn is_float_literal(s string) bool {
	if !s.contains('.') {
		return false
	}
	parts := s.split('.')
	if parts.len != 2 {
		return false
	}
	return is_integer_literal(parts[0]) && is_integer_literal(parts[1])
}

// Cria um TypeInfo para tuplas
pub fn typeinfo_from_tuple(tuple_type string) TypeInfo {
	return TypeInfo{
		generic: 'tuple'
		value:   tuple_type
	}
}

// Extrai elementos de uma tupla a partir da string de tipo
pub fn extract_tuple_elements(tuple_type string) []string {
	content := tuple_type.trim_string_left('{').trim_string_right('}')
	mut elements := []string{}
	mut current := ''
	mut brace_count := 0
	for c in content {
		if c == `{` {
			brace_count++
		} else if c == `}` {
			brace_count--
		} else if c == `,` && brace_count == 0 {
			elements << current.trim_space()
			current = ''
			continue
		}
		current += c.ascii_str()
	}
	if current.len > 0 {
		elements << current.trim_space()
	}
	return elements
}

// Rules:
// 1. Different generic types are incompatible (except unions)
// 2. If both have values, they must match exactly (or recursively for composites)
// 3. If only one has value: generic accepts specific, specific rejects generic
pub fn types_are_compatible(expected TypeInfo, actual TypeInfo) bool {
	// Rule 1: Different generic types are incompatible
	if expected.generic != actual.generic {
		// Special case: 'any' type accepts anything
		if expected.generic == 'any' || actual.generic == 'any' {
			return true
		}
		return false
	}

	// Rule 2: Both have specific values
	if expected.value != none && actual.value != none {
		// For now, use a simple string comparison approach
		expected_str := expected.str()
		actual_str := actual.str()

		// Handle composite types recursively
		if expected.generic == 'tuple' {
			return tuple_types_are_compatible(expected_str, actual_str)
		}
		if expected.generic == 'list' {
			return expected_str == actual_str // Simplified for now
		}
		if expected.generic == 'map' {
			return expected_str == actual_str // Simplified for now
		}

		// Simple value comparison
		return expected_str == actual_str
	}

	// Rule 3: Only one has value
	if expected.value == none && actual.value != none {
		// Generic expected accepts specific actual
		return true
	}
	if expected.value != none && actual.value == none {
		// Specific expected rejects generic actual
		return false
	}

	// Both are generic - compatible
	return true
}

pub fn tuple_types_are_compatible(tuple1 string, tuple2 string) bool {
	elements1 := extract_tuple_elements(tuple1)
	elements2 := extract_tuple_elements(tuple2)
	if elements1.len != elements2.len {
		return false
	}
	for i in 0 .. elements1.len {
		// Simple string comparison to avoid infinite recursion
		if elements1[i] != elements2[i] {
			return false
		}
	}
	return true
}

pub fn list_types_are_compatible(list1 string, list2 string) bool {
	// Extract element types from list(element_type) format
	elem_type1 := extract_list_element_type(list1)
	elem_type2 := extract_list_element_type(list2)

	type1 := typeinfo_from_str(elem_type1)
	type2 := typeinfo_from_str(elem_type2)

	return types_are_compatible(type1, type2)
}

pub fn map_types_are_compatible(map1 string, map2 string) bool {
	// Extract key=>value types from map(key=>value) format
	key_val1 := extract_map_key_value_types(map1)
	key_val2 := extract_map_key_value_types(map2)

	if key_val1.len != 2 || key_val2.len != 2 {
		return false
	}

	key_type1 := typeinfo_from_str(key_val1[0])
	key_type2 := typeinfo_from_str(key_val2[0])
	val_type1 := typeinfo_from_str(key_val1[1])
	val_type2 := typeinfo_from_str(key_val2[1])

	return types_are_compatible(key_type1, key_type2) && types_are_compatible(val_type1, val_type2)
}

// Extract element type from list(element_type) format
pub fn extract_list_element_type(list_type string) string {
	if list_type.starts_with('list(') && list_type.ends_with(')') {
		return list_type[5..list_type.len - 1]
	}
	return 'any'
}

// Extract key and value types from map(key=>value) format
pub fn extract_map_key_value_types(map_type string) []string {
	if map_type.starts_with('map(') && map_type.ends_with(')') {
		content := map_type[4..map_type.len - 1]
		parts := content.split('=>')
		if parts.len == 2 {
			return [parts[0].trim_space(), parts[1].trim_space()]
		}
	}
	return ['any', 'any']
}

// Basic conversion functions (without external dependencies)
pub fn typeinfo_to_string(ti TypeInfo) string {
	return ti.str()
}

// Create TypeInfo from AST literal
pub fn typeinfo_from_literal(literal ast.Literal) TypeInfo {
	match literal {
		ast.IntegerLiteral {
			return TypeInfo{
				generic: 'integer'
				value:   literal.value.str()
			}
		}
		ast.FloatLiteral {
			return TypeInfo{
				generic: 'float'
				value:   literal.value.str()
			}
		}
		ast.StringLiteral {
			return TypeInfo{
				generic: 'string'
				value:   '"${literal.value}"'
			}
		}
		ast.BooleanLiteral {
			return TypeInfo{
				generic: 'boolean'
				value:   literal.value.str()
			}
		}
		ast.AtomLiteral {
			return TypeInfo{
				generic: 'atom'
				value:   literal.value
			}
		}
		ast.NilLiteral {
			return TypeInfo{
				generic: 'nil'
				value:   none
			}
		}
	}
}
