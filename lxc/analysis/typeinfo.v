module analysis

import ast

// TypeInfo representa um tipo com base genérica e valor específico (quando possível)
pub struct TypeInfo {
pub mut:
	generic string  // 'atom', 'integer', 'string', 'union', 'list', 'map', 'tuple', etc.
	value   ?string // ':ok', '1', etc. ou none
	values  []TypeInfo // Para union types, list element types, map key/value types, tuple element types
}

// Convert TypeInfo to string representation
pub fn (ti TypeInfo) str() string {
	if ti.generic == 'union' && ti.values.len > 0 {
		// Union types: join all values with |
		return ti.values.map(it.str()).join(' | ')
	}

	if ti.generic == 'list' && ti.values.len > 0 {
		// List types: [element_type]
		return '[${ti.values[0].str()}]'
	}

	if ti.generic == 'map' && ti.values.len >= 2 {
		// Map types: #{key_type => value_type}
		return '#{${ti.values[0].str()} => ${ti.values[1].str()}}'
	}

	if ti.generic == 'tuple' && ti.values.len > 0 {
		// Tuple types: {element1, element2, ...}
		return '{${ti.values.map(it.str()).join(', ')}}'
	}

	if value := ti.value {
		// Special handling for tuples - return just the value without tuple() prefix
		if ti.generic == 'tuple' {
			return value
		}
		return '${ti.generic}(${value})'
	}
	return ti.generic
}

// Create TypeInfo for basic types
pub fn typeinfo_integer() TypeInfo {
	return TypeInfo{
		generic: 'integer'
		value:   none
		values:  []
	}
}

pub fn typeinfo_float() TypeInfo {
	return TypeInfo{
		generic: 'float'
		value:   none
		values:  []
	}
}

pub fn typeinfo_string() TypeInfo {
	return TypeInfo{
		generic: 'string'
		value:   none
		values:  []
	}
}

pub fn typeinfo_boolean() TypeInfo {
	return TypeInfo{
		generic: 'boolean'
		value:   none
		values:  []
	}
}

pub fn typeinfo_atom() TypeInfo {
	return TypeInfo{
		generic: 'atom'
		value:   none
		values:  []
	}
}

pub fn typeinfo_nil() TypeInfo {
	return TypeInfo{
		generic: 'nil'
		value:   none
		values:  []
	}
}

pub fn typeinfo_any() TypeInfo {
	return TypeInfo{
		generic: 'any'
		value:   none
		values:  []
	}
}

// Create TypeInfo for specific values
pub fn typeinfo_integer_value(value int) TypeInfo {
	return TypeInfo{
		generic: 'integer'
		value:   value.str()
		values:  []
	}
}

pub fn typeinfo_atom_value(value string) TypeInfo {
	return TypeInfo{
		generic: 'atom'
		value:   value
		values:  []
	}
}

pub fn typeinfo_string_value(value string) TypeInfo {
	return TypeInfo{
		generic: 'string'
		value:   value
		values:  []
	}
}

pub fn typeinfo_boolean_value(value bool) TypeInfo {
	return TypeInfo{
		generic: 'boolean'
		value:   value.str()
		values:  []
	}
}

// Create TypeInfo for complex types
pub fn typeinfo_union(types []TypeInfo) TypeInfo {
	return TypeInfo{
		generic: 'union'
		value:   none
		values:  types
	}
}

pub fn typeinfo_list(element_type TypeInfo) TypeInfo {
	return TypeInfo{
		generic: 'list'
		value:   none
		values:  [element_type]
	}
}

pub fn typeinfo_map(key_type TypeInfo, value_type TypeInfo) TypeInfo {
	return TypeInfo{
		generic: 'map'
		value:   none
		values:  [key_type, value_type]
	}
}

pub fn typeinfo_tuple(element_types []TypeInfo) TypeInfo {
	return TypeInfo{
		generic: 'tuple'
		value:   none
		values:  element_types
	}
}

pub fn typeinfo_record(name string) TypeInfo {
	return TypeInfo{
		generic: 'record'
		value:   name
		values:  []
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
		values:  []
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

	// Create TypeInfo directly instead of using typeinfo_from_str
	elem_type_info1 := typeinfo_from_list_string(elem_type1)
	elem_type_info2 := typeinfo_from_list_string(elem_type2)

	return types_are_compatible(elem_type_info1, elem_type_info2)
}

pub fn map_types_are_compatible(map1 string, map2 string) bool {
	// Extract key=>value types from map(key=>value) format
	key_val1 := extract_map_key_value_types(map1)
	key_val2 := extract_map_key_value_types(map2)

	if key_val1.len != 2 || key_val2.len != 2 {
		return false
	}

	// Create TypeInfo directly instead of using typeinfo_from_str
	key_type1 := typeinfo_from_basic_string(key_val1[0])
	key_type2 := typeinfo_from_basic_string(key_val2[0])
	val_type1 := typeinfo_from_basic_string(key_val1[1])
	val_type2 := typeinfo_from_basic_string(key_val2[1])

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

// Helper functions to create TypeInfo from basic strings (for legacy compatibility)
pub fn typeinfo_from_basic_string(type_str string) TypeInfo {
	match type_str {
		'integer', 'integer()' { return typeinfo_integer() }
		'float', 'float()' { return typeinfo_float() }
		'string', 'string()' { return typeinfo_string() }
		'boolean', 'boolean()' { return typeinfo_boolean() }
		'atom', 'atom()' { return typeinfo_atom() }
		'nil' { return typeinfo_nil() }
		'any', 'any()' { return typeinfo_any() }
		else { return typeinfo_any() }
	}
}

fn typeinfo_from_list_string(element_type_str string) TypeInfo {
	element_type := typeinfo_from_basic_string(element_type_str)
	return typeinfo_list(element_type)
}

// Basic conversion functions (without external dependencies)
pub fn typeinfo_to_string(ti TypeInfo) string {
	return ti.str()
}

// Create TypeInfo from AST literal
pub fn typeinfo_from_literal(literal ast.Literal) TypeInfo {
	match literal {
		ast.IntegerLiteral {
			return typeinfo_integer_value(literal.value)
		}
		ast.FloatLiteral {
			return TypeInfo{
				generic: 'float'
				value:   literal.value.str()
				values:  []
			}
		}
		ast.StringLiteral {
			return typeinfo_string_value(literal.value)
		}
		ast.BooleanLiteral {
			return typeinfo_boolean_value(literal.value)
		}
		ast.AtomLiteral {
			return typeinfo_atom_value(literal.value)
		}
		ast.NilLiteral {
			return typeinfo_nil()
		}
	}
}
