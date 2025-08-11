module analysis

import ast

pub type TypeExpr = TypeVar
	| TypeConstructor
	| FunctionType
	| RecordType
	| MapType
	| TupleType
	| ListType
	| BinaryType
	| UnionType

pub struct TypeVar {
pub:
	name string
}

pub struct TypeConstructor {
pub:
	name       string
	parameters []TypeExpr
}

pub struct FunctionType {
pub:
	parameters  []TypeExpr
	return_type TypeExpr
}

pub struct RecordType {
pub:
	name   string
	fields map[string]TypeExpr
}

pub struct MapType {
pub:
	key_type   TypeExpr
	value_type TypeExpr
}

pub struct TupleType {
pub:
	elements []TypeExpr
}

pub struct ListType {
pub:
	element_type TypeExpr
}

pub struct BinaryType {
pub:
	unit_size int
}

pub struct UnionType {
pub:
	types []TypeExpr
}

pub struct TypeBinding {
pub:
	name      string
	type_info TypeInfo // Updated to use TypeInfo instead of TypeExpr
	position  ast.Position
}

pub struct TypeContext {
pub mut:
	bindings              map[string]TypeBinding
	type_aliases          map[string]TypeExpr
	record_types          map[string]string
	function_contexts     map[string]&TypeContext
	expression_types      map[string]TypeInfo   // Store expression types by their string representation
	function_return_types map[string]TypeInfo   // Store function return types by func_name/arity
	function_param_types  map[string][]TypeInfo // Store function parameter types by func_name/arity/params
	parent                ?&TypeContext
	level                 int
}

pub fn new_type_context() TypeContext {
	return TypeContext{
		bindings:              map[string]TypeBinding{}
		type_aliases:          map[string]TypeExpr{}
		record_types:          map[string]string{}
		function_contexts:     map[string]&TypeContext{}
		expression_types:      map[string]TypeInfo{}
		parent:                none
		level:                 0
		function_return_types: map[string]TypeInfo{}
		function_param_types:  map[string][]TypeInfo{}
	}
}

pub fn (tc &TypeContext) lookup(name string) ?TypeBinding {
	if name in tc.bindings {
		return tc.bindings[name]
	}
	if parent := tc.parent {
		return parent.lookup(name)
	}
	return none
}

pub fn (mut tc TypeContext) bind(name string, type_info TypeInfo, position ast.Position) {
	tc.bindings[name] = TypeBinding{
		name:      name
		type_info: type_info
		position:  position
	}
}

// Legacy function for backwards compatibility - simplified without external dependencies
pub fn (mut tc TypeContext) bind_string(name string, type_str string, position ast.Position) {
	type_info := typeinfo_from_basic_string(type_str)
	tc.bind(name, type_info, position)
}

pub fn (tc &TypeContext) enter_function(name string) TypeContext {
	mut new_context := TypeContext{
		bindings:              map[string]TypeBinding{}
		type_aliases:          map[string]TypeExpr{}
		record_types:          map[string]string{}
		function_contexts:     map[string]&TypeContext{}
		parent:                unsafe { tc }
		level:                 tc.level + 1
		function_return_types: map[string]TypeInfo{}
		function_param_types:  map[string][]TypeInfo{}
	}

	// Copy function return types from parent context
	for key, value in tc.function_return_types {
		new_context.function_return_types[key] = value
	}

	// Copy function param types from parent context
	for key, value in tc.function_param_types {
		new_context.function_param_types[key] = value
	}

	return new_context
}

pub fn (tc &TypeContext) exit_function() ?&TypeContext {
	return tc.parent
}

// Simplified context information getter - no external dependencies
pub fn (tc &TypeContext) get_context_info() map[string]string {
	mut info := map[string]string{}

	// Add bindings info
	for k, v in tc.bindings {
		info['binding_${k}'] = v.type_info.str()
	}

	// Add record types info
	for k, v in tc.record_types {
		info['record_${k}'] = v
	}

	return info
}

// Get expression type as TypeInfo
pub fn (tc &TypeContext) get_expression_type(expr ast.Expr) ?TypeInfo {
	// Create a unique key for the expression
	expr_key := expr.str()

	// Look in current context
	if type_info := tc.expression_types[expr_key] {
		return type_info
	}

	// Look in parent context
	if parent := tc.parent {
		return parent.get_expression_type(expr)
	}

	return none
}

// Store expression type
pub fn (mut tc TypeContext) store_expression_type(expr ast.Expr, type_info TypeInfo) {
	expr_key := expr.str()
	tc.expression_types[expr_key] = type_info
}

// Store function return type
pub fn (mut tc TypeContext) store_function_return_type(func_name string, arity int, return_type TypeInfo) {
	key := '${func_name}/${arity}'
	tc.function_return_types[key] = return_type
}

// get_function_return_type gets the return type for a function
pub fn (tc &TypeContext) get_function_return_type(func_name string, arity int) ?TypeInfo {
	key := '${func_name}/${arity}'
	if type_info := tc.function_return_types[key] {
		return type_info
	}
	return none
}

// Store function parameter types for union generation
pub fn (mut tc TypeContext) store_function_param_types(func_name string, arity int, param_types []TypeInfo) {
	key := '${func_name}/${arity}/params'
	tc.function_param_types[key] = param_types
}

// Get function parameter types
pub fn (tc &TypeContext) get_function_param_types(func_name string, arity int) ?[]TypeInfo {
	key := '${func_name}/${arity}/params'
	return tc.function_param_types[key]
}

// Merge parameter types to create unions
pub fn (mut tc TypeContext) merge_function_param_types(func_name string, arity int, new_param_types []TypeInfo) {
	key := '${func_name}/${arity}/params'
	if existing := tc.function_param_types[key] {
		// Merge existing and new parameter types
		mut merged := []TypeInfo{}
		for i in 0 .. existing.len {
			if i < new_param_types.len {
				// Create union of existing and new type for this parameter
				union_type := create_union_type(existing[i], new_param_types[i])
				merged << union_type
			} else {
				merged << existing[i]
			}
		}
		// Add any new parameters beyond existing count
		for i in existing.len .. new_param_types.len {
			merged << new_param_types[i]
		}
		tc.function_param_types[key] = merged
	} else {
		// First time storing parameter types for this function/arity
		tc.function_param_types[key] = new_param_types
	}
}

// Merge return types to create unions
pub fn (mut tc TypeContext) merge_function_return_type(func_name string, arity int, new_return_type TypeInfo) {
	key := '${func_name}/${arity}'
	if existing := tc.function_return_types[key] {
		// Special case: if existing is 'any', replace with new type
		if existing.generic == 'any' {
			tc.function_return_types[key] = new_return_type
		} else {
			// Create union of existing and new return type
			union_type := create_union_type(existing, new_return_type)
			tc.function_return_types[key] = union_type
		}
	} else {
		// First time storing return type for this function/arity
		tc.function_return_types[key] = new_return_type
	}
}

// Create union type from two types
pub fn create_union_type(type1 TypeInfo, type2 TypeInfo) TypeInfo {
	if types_are_compatible(type1, type2) {
		return type1 // If compatible, return the first type
	}
	// Cria union usando o campo values corretamente
	mut types := []TypeInfo{}
	// Se type1 já é union, concatena
	if type1.generic == 'union' {
		types << type1.values
	} else {
		types << type1
	}
	if type2.generic == 'union' {
		types << type2.values
	} else {
		types << type2
	}
	return TypeInfo{
		generic: 'union'
		value:   none
		values:  types
	}
}

// Helper function to get type string representation
fn type_to_string(te TypeExpr) string {
	match te {
		TypeConstructor {
			if te.parameters.len == 0 {
				return te.name
			}
			param_strs := te.parameters.map(type_to_string(it))
			return '${te.name}(${param_strs.join(', ')})'
		}
		ListType {
			return 'list(${type_to_string(te.element_type)})'
		}
		TupleType {
			element_strs := te.elements.map(type_to_string(it))
			return '{${element_strs.join(', ')}}'
		}
		MapType {
			return 'map(${type_to_string(te.key_type)}=>${type_to_string(te.value_type)})'
		}
		TypeVar {
			return te.name
		}
		else {
			return 'unknown'
		}
	}
}
