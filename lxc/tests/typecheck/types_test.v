module main

import analysis.typechecker
import frontend.parser
import frontend.lexer
import ast

fn test_type_variables() {
	// Test type variable creation and string representation
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	assert tv1.str() == 'T1'

	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}
	assert tv2.str() == 'T2'

	// Test type variable with type expressions
	assert typechecker.is_monomorphic_type_var(tv1) == false
	assert typechecker.contains_type_var_type_var(tv1, 'T1') == true
	assert typechecker.contains_type_var_type_var(tv1, 'T2') == false
	assert typechecker.get_type_vars_type_var(tv1) == ['T1']
}

fn test_type_constructors() {
	// Test basic type constructors
	assert typechecker.integer_type.str() == 'integer'
	assert typechecker.string_type.str() == 'string'
	assert typechecker.boolean_type.str() == 'boolean'

	// Test type constructors with parameters
	list_int := typechecker.TypeConstructor{
		name:       'list'
		parameters: [typechecker.integer_type]
	}
	assert list_int.str() == 'list(integer)'

	// Test monomorphic check
	assert typechecker.is_monomorphic_type_constructor(typechecker.integer_type) == true
	assert typechecker.is_monomorphic_type_constructor(list_int) == true

	// Test type variable containment
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	list_tv := typechecker.TypeConstructor{
		name:       'list'
		parameters: [tv]
	}
	assert typechecker.is_monomorphic_type_constructor(list_tv) == false
	assert typechecker.contains_type_var_type_constructor(list_tv, 'T1') == true
	assert typechecker.get_type_vars_type_constructor(list_tv) == ['T1']
}

fn test_function_types() {
	// Test function type creation
	fn_type := typechecker.make_function_type([typechecker.integer_type, typechecker.string_type],
		typechecker.boolean_type)
	assert fn_type.str() == '(integer, string) -> boolean'

	// Test monomorphic check
	assert typechecker.is_monomorphic_function_type(fn_type) == true

	// Test with type variables
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}
	polymorphic_fn := typechecker.make_function_type([tv1], tv2)
	assert typechecker.is_monomorphic_function_type(polymorphic_fn) == false
	assert typechecker.contains_type_var_function_type(polymorphic_fn, 'T1') == true
	assert typechecker.contains_type_var_function_type(polymorphic_fn, 'T2') == true
	assert typechecker.get_type_vars_function_type(polymorphic_fn) == ['T1', 'T2']
}

fn test_record_types() {
	// Test record type creation
	person_record := typechecker.RecordType{
		name:   'Person'
		fields: {
			'name': typechecker.string_type
			'age':  typechecker.integer_type
		}
	}
	assert person_record.str() == 'Person{name: string, age: integer}'

	// Test monomorphic check
	assert typechecker.is_monomorphic_record_type(person_record) == true

	// Test with type variables
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	generic_record := typechecker.RecordType{
		name:   'Container'
		fields: {
			'value': tv
		}
	}
	assert typechecker.is_monomorphic_record_type(generic_record) == false
	assert typechecker.contains_type_var_record_type(generic_record, 'T1') == true
	assert typechecker.get_type_vars_record_type(generic_record) == ['T1']
}

fn test_map_types() {
	// Test map type creation
	string_int_map := typechecker.make_map_type(typechecker.string_type, typechecker.integer_type)
	assert string_int_map.str() == 'map(string, integer)'

	// Test monomorphic check
	assert typechecker.is_monomorphic_map_type(string_int_map) == true

	// Test with type variables
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}
	generic_map := typechecker.make_map_type(tv1, tv2)
	assert typechecker.is_monomorphic_map_type(generic_map) == false
	assert typechecker.contains_type_var_map_type(generic_map, 'T1') == true
	assert typechecker.contains_type_var_map_type(generic_map, 'T2') == true
	assert typechecker.get_type_vars_map_type(generic_map) == ['T1', 'T2']
}

fn test_tuple_types() {
	// Test tuple type creation
	tuple_type := typechecker.make_tuple_type([typechecker.integer_type, typechecker.string_type,
		typechecker.boolean_type])
	assert tuple_type.str() == '(integer, string, boolean)'

	// Test monomorphic check
	assert typechecker.is_monomorphic_tuple_type(tuple_type) == true

	// Test with type variables
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	generic_tuple := typechecker.make_tuple_type([tv, typechecker.integer_type])
	assert typechecker.is_monomorphic_tuple_type(generic_tuple) == false
	assert typechecker.contains_type_var_tuple_type(generic_tuple, 'T1') == true
	assert typechecker.get_type_vars_tuple_type(generic_tuple) == ['T1']
}

fn test_list_types() {
	// Test list type creation
	int_list := typechecker.make_list_type(typechecker.integer_type)
	assert int_list.str() == 'list(integer)'

	// Test monomorphic check
	assert typechecker.is_monomorphic_list_type(int_list) == true

	// Test with type variables
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	generic_list := typechecker.make_list_type(tv)
	assert typechecker.is_monomorphic_list_type(generic_list) == false
	assert typechecker.contains_type_var_list_type(generic_list, 'T1') == true
	assert typechecker.get_type_vars_list_type(generic_list) == ['T1']
}

fn test_binary_types() {
	// Test binary type creation
	binary_type := typechecker.BinaryType{
		unit_size: 0
	}
	assert binary_type.str() == 'binary'

	binary_8 := typechecker.BinaryType{
		unit_size: 8
	}
	assert binary_8.str() == 'binary(8)'

	// Test monomorphic check
	assert typechecker.is_monomorphic_binary_type(binary_type) == true
	assert typechecker.is_monomorphic_binary_type(binary_8) == true

	// Test type variable containment
	assert typechecker.contains_type_var_binary_type(binary_type, 'T1') == false
	assert typechecker.get_type_vars_binary_type(binary_type) == []string{}
}

fn test_complex_type_expressions() {
	// Test complex nested type expressions
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}

	// List of functions
	list_of_fns := typechecker.make_list_type(typechecker.make_function_type([tv1], tv2))
	assert list_of_fns.str() == 'list((T1) -> T2)'
	assert typechecker.is_monomorphic_list_type(list_of_fns) == false
	assert typechecker.get_type_vars_list_type(list_of_fns) == ['T1', 'T2']

	// Tuple with records
	person_record := typechecker.RecordType{
		name:   'Person'
		fields: {
			'name': typechecker.string_type
			'age':  typechecker.integer_type
		}
	}

	complex_tuple := typechecker.make_tuple_type([
		person_record,
		typechecker.make_list_type(typechecker.string_type),
		typechecker.make_map_type(typechecker.atom_type, typechecker.integer_type),
	])
	assert complex_tuple.str() == '(Person{name: string, age: integer}, list(string), map(atom, integer))'
	assert typechecker.is_monomorphic_tuple_type(complex_tuple) == true
}

fn test_builtin_types() {
	// Test all built-in types
	assert typechecker.integer_type.str() == 'integer'
	assert typechecker.float_type.str() == 'float'
	assert typechecker.string_type.str() == 'string'
	assert typechecker.boolean_type.str() == 'boolean'
	assert typechecker.atom_type.str() == 'atom'
	assert typechecker.nil_type.str() == 'nil'
	assert typechecker.any_type.str() == 'any'
	assert typechecker.unknown_type.str() == 'unknown'

	// All built-in types should be monomorphic
	assert typechecker.is_monomorphic_type_constructor(typechecker.integer_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.float_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.string_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.boolean_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.atom_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.nil_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.any_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.unknown_type) == true
}

fn test_type_equality() {
	// Test type equality
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}

	// Same type variables should be equal
	assert typechecker.equals_type_var(tv1, tv1) == true
	assert typechecker.equals_type_var(tv2, tv2) == true
	assert typechecker.equals_type_var(tv1, tv2) == false

	// Same type constructors should be equal
	assert typechecker.equals_type_constructor(typechecker.integer_type, typechecker.integer_type) == true
	assert typechecker.equals_type_constructor(typechecker.string_type, typechecker.string_type) == true
	assert typechecker.equals_type_constructor(typechecker.integer_type, typechecker.string_type) == false

	// Same function types should be equal
	fn1 := typechecker.make_function_type([typechecker.integer_type], typechecker.string_type)
	fn2 := typechecker.make_function_type([typechecker.integer_type], typechecker.string_type)
	assert typechecker.equals_function_type(fn1, fn2) == true

	// Different function types should not be equal
	fn3 := typechecker.make_function_type([typechecker.string_type], typechecker.integer_type)
	assert typechecker.equals_function_type(fn1, fn3) == false
}

fn test_type_variables_in_types() {
	// Test finding type variables in complex types
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}

	// Simple type variable
	assert typechecker.contains_type_var_type_var(tv1, 'T1') == true
	assert typechecker.contains_type_var_type_var(tv1, 'T2') == false

	// Function type with type variables
	fn_type := typechecker.make_function_type([tv1], tv2)
	assert typechecker.contains_type_var_function_type(fn_type, 'T1') == true
	assert typechecker.contains_type_var_function_type(fn_type, 'T2') == true
	assert typechecker.contains_type_var_function_type(fn_type, 'T3') == false

	// List type with type variable
	list_tv := typechecker.make_list_type(tv1)
	assert typechecker.contains_type_var_list_type(list_tv, 'T1') == true
	assert typechecker.contains_type_var_list_type(list_tv, 'T2') == false
}

fn test_type_variable_collection() {
	// Test collecting all type variables from types
	tv1 := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}
	tv2 := typechecker.TypeVar{
		id:   'T2'
		name: ''
	}

	// Simple type variable
	assert typechecker.get_type_vars_type_var(tv1) == ['T1']

	// Function type with multiple type variables
	fn_type := typechecker.make_function_type([tv1], tv2)
	type_vars := typechecker.get_type_vars_function_type(fn_type)
	assert type_vars.len == 2
	assert 'T1' in type_vars
	assert 'T2' in type_vars

	// List type with type variable
	list_tv := typechecker.make_list_type(tv1)
	assert typechecker.get_type_vars_list_type(list_tv) == ['T1']
}

fn test_type_monomorphism() {
	// Test monomorphism checking
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}

	// Type variables are not monomorphic
	assert typechecker.is_monomorphic_type_var(tv) == false

	// Built-in types are monomorphic
	assert typechecker.is_monomorphic_type_constructor(typechecker.integer_type) == true
	assert typechecker.is_monomorphic_type_constructor(typechecker.string_type) == true

	// Function types with type variables are not monomorphic
	fn_type := typechecker.make_function_type([tv], typechecker.integer_type)
	assert typechecker.is_monomorphic_function_type(fn_type) == false

	// Function types without type variables are monomorphic
	fn_type2 := typechecker.make_function_type([typechecker.integer_type], typechecker.string_type)
	assert typechecker.is_monomorphic_function_type(fn_type2) == true
}

fn test_type_string_representations() {
	// Test string representations of various types
	tv := typechecker.TypeVar{
		id:   'T1'
		name: ''
	}

	// Type variable
	assert tv.str() == 'T1'

	// Type constructor
	assert typechecker.integer_type.str() == 'integer'

	// Function type
	fn_type := typechecker.make_function_type([typechecker.integer_type], typechecker.string_type)
	assert fn_type.str() == '(integer) -> string'

	// Record type
	record_type := typechecker.RecordType{
		name:   'Person'
		fields: {
			'name': typechecker.string_type
			'age':  typechecker.integer_type
		}
	}
	assert record_type.str() == 'Person{name: string, age: integer}'

	// Map type
	map_type := typechecker.make_map_type(typechecker.string_type, typechecker.integer_type)
	assert map_type.str() == 'map(string, integer)'

	// Tuple type
	tuple_type := typechecker.make_tuple_type([
		typechecker.integer_type,
		typechecker.string_type,
	])
	assert tuple_type.str() == '(integer, string)'

	// List type
	list_type := typechecker.make_list_type(typechecker.integer_type)
	assert list_type.str() == 'list(integer)'

	// Binary type
	binary_type := typechecker.BinaryType{
		unit_size: 8
	}
	assert binary_type.str() == 'binary(8)'
}

fn test_parse_boolean_literal() {
	source := 'true'

	// Create lexer and tokenize
	mut lexer_instance := lexer.new_lexer(source, 'test.lx')
	mut tokens := []lexer.Token{}
	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			panic('Lexical error: ${token.message}')
		}
		tokens << token
	}

	// Verify we have the right token
	assert tokens.len == 1
	assert tokens[0] is lexer.KeywordToken
	keyword_token := tokens[0] as lexer.KeywordToken
	assert keyword_token.value == lexer.KeywordValue.true_

	// Test parser primary expression
	mut expr_parser := parser.new_expression_parser(tokens)
	expr := expr_parser.parse_expression() or { panic('Failed to parse boolean literal') }

	// Verify the AST structure
	assert expr is ast.LiteralExpr
	lit := expr as ast.LiteralExpr
	assert lit.value is ast.BooleanLiteral
	bool_lit := lit.value as ast.BooleanLiteral
	assert bool_lit.value == true

	println('Boolean literal parsing test passed!')
}

fn main() {
	test_type_variables()
	test_type_constructors()
	test_function_types()
	test_record_types()
	test_map_types()
	test_tuple_types()
	test_list_types()
	test_binary_types()
	test_type_equality()
	test_type_variables_in_types()
	test_type_variable_collection()
	test_type_monomorphism()
	test_type_string_representations()
	test_complex_type_expressions()
	test_builtin_types()
	test_parse_boolean_literal()
}
