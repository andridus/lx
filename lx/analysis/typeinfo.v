module analysis

import ast

pub fn type_integer() ast.Type {
	return ast.Type{
		name:   'integer'
		params: []
	}
}

pub fn type_float() ast.Type {
	return ast.Type{
		name:   'float'
		params: []
	}
}

pub fn type_string() ast.Type {
	return ast.Type{
		name:   'string'
		params: []
	}
}

pub fn type_boolean() ast.Type {
	return ast.Type{
		name:   'boolean'
		params: []
	}
}

pub fn type_atom() ast.Type {
	return ast.Type{
		name:   'atom'
		params: []
	}
}

pub fn type_nil() ast.Type {
	return ast.Type{
		name:   'nil'
		params: []
	}
}

pub fn type_function(param_types []ast.Type, return_type ast.Type) ast.Type {
	mut params := param_types.clone()
	params << return_type
	return ast.Type{
		name:   'function'
		params: params
	}
}

pub fn type_module() ast.Type {
	return ast.Type{
		name:   'module'
		params: []
	}
}

pub fn type_to_string(t ast.Type) string {
	return t.str()
}

// Specialized type constructors
pub fn type_specialized_atom(atom_value string) ast.Type {
	return ast.Type{
		name:              'atom'
		params:            []
		specialized_value: atom_value
	}
}

pub fn type_specialized_integer(int_value string) ast.Type {
	return ast.Type{
		name:              'integer'
		params:            []
		specialized_value: int_value
	}
}

pub fn type_specialized_string(str_value string) ast.Type {
	return ast.Type{
		name:              'string'
		params:            []
		specialized_value: str_value
	}
}
