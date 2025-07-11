module analysis1

pub const integer_type = TypeConstructor{
	name:       'integer'
	parameters: []
}
pub const float_type = TypeConstructor{
	name:       'float'
	parameters: []
}
pub const string_type = TypeConstructor{
	name:       'string'
	parameters: []
}
pub const boolean_type = TypeConstructor{
	name:       'boolean'
	parameters: []
}
pub const atom_type = TypeConstructor{
	name:       'atom'
	parameters: []
}
pub const nil_type = TypeConstructor{
	name:       'nil'
	parameters: []
}
pub const any_type = TypeConstructor{
	name:       'any'
	parameters: []
}

pub fn make_type_var(name string) TypeVar {
	return TypeVar{
		name: name
	}
}

pub fn make_type_constructor(name string, parameters []TypeExpr) TypeConstructor {
	return TypeConstructor{
		name:       name
		parameters: parameters
	}
}

pub fn make_function_type(parameters []TypeExpr, return_type TypeExpr) FunctionType {
	return FunctionType{
		parameters:  parameters
		return_type: return_type
	}
}

pub fn make_record_type(name string, fields map[string]TypeExpr) RecordType {
	return RecordType{
		name:   name
		fields: fields
	}
}

pub fn make_map_type(key_type TypeExpr, value_type TypeExpr) MapType {
	return MapType{
		key_type:   key_type
		value_type: value_type
	}
}

pub fn make_tuple_type(elements []TypeExpr) TupleType {
	return TupleType{
		elements: elements
	}
}

pub fn make_list_type(element_type TypeExpr) ListType {
	return ListType{
		element_type: element_type
	}
}

pub fn make_binary_type(unit_size int) BinaryType {
	return BinaryType{
		unit_size: unit_size
	}
}

pub fn make_union_type(types []TypeExpr) UnionType {
	return UnionType{
		types: types
	}
}

pub fn (tc TypeConstructor) str() string {
	if tc.parameters.len == 0 {
		return tc.name
	}
	param_strs := tc.parameters.map(it.str())
	return '${tc.name}(${param_strs.join(', ')})'
}

pub fn (tv TypeVar) str() string {
	return tv.name
}

pub fn (ft FunctionType) str() string {
	param_strs := ft.parameters.map(it.str())
	return '(${param_strs.join(', ')}) -> ${ft.return_type.str()}'
}

pub fn (rt RecordType) str() string {
	mut field_strs := []string{}
	for field_name, field_type in rt.fields {
		field_strs << '${field_name}: ${field_type.str()}'
	}
	return 'record ${rt.name} { ${field_strs.join(', ')} }'
}

pub fn (mt MapType) str() string {
	return 'map<${mt.key_type.str()}, ${mt.value_type.str()}>'
}

pub fn (tt TupleType) str() string {
	element_strs := tt.elements.map(it.str())
	return '(${element_strs.join(', ')})'
}

pub fn (lt ListType) str() string {
	return '[${lt.element_type.str()}]'
}

pub fn (bt BinaryType) str() string {
	return 'binary(${bt.unit_size})'
}

pub fn (ut UnionType) str() string {
	type_strs := ut.types.map(it.str())
	return type_strs.join(' | ')
}
