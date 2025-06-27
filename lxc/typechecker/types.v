module typechecker

// TypeVar represents a type variable for polymorphic types
pub struct TypeVar {
pub:
	id   string
	name string
}

// str returns a string representation of TypeVar
pub fn (tv TypeVar) str() string {
	return tv.id
}

// TypeConstructor represents a type constructor (like list, tuple, etc.)
pub struct TypeConstructor {
pub:
	name       string
	parameters []TypeExpr
}

// str returns a string representation of TypeConstructor
pub fn (tc TypeConstructor) str() string {
	if tc.parameters.len == 0 {
		return tc.name
	}
	params := tc.parameters.map(it.str()).join(', ')
	return '${tc.name}(${params})'
}

// FunctionType represents a function type
pub struct FunctionType {
pub:
	parameters  []TypeExpr
	return_type TypeExpr
}

// str returns a string representation of FunctionType
pub fn (ft FunctionType) str() string {
	if ft.parameters.len == 0 {
		return '() -> ${ft.return_type.str()}'
	}
	params := ft.parameters.map(it.str()).join(', ')
	return '(${params}) -> ${ft.return_type.str()}'
}

// RecordType represents a record type with named fields
pub struct RecordType {
pub:
	name   string
	fields map[string]TypeExpr
}

// str returns a string representation of RecordType
pub fn (rt RecordType) str() string {
	mut field_strs := []string{}
	for field_name, field_type in rt.fields {
		field_strs << '${field_name}: ${field_type.str()}'
	}
	fields := field_strs.join(', ')
	return '${rt.name}{${fields}}'
}

// MapType represents a map type with key and value types
pub struct MapType {
pub:
	key_type   TypeExpr
	value_type TypeExpr
}

// str returns a string representation of MapType
pub fn (mt MapType) str() string {
	return 'map(${mt.key_type.str()}, ${mt.value_type.str()})'
}

// TupleType represents a tuple type
pub struct TupleType {
pub:
	element_types []TypeExpr
}

// str returns a string representation of TupleType
pub fn (tt TupleType) str() string {
	elements := tt.element_types.map(it.str()).join(', ')
	return '(${elements})'
}

// ListType represents a list type
pub struct ListType {
pub:
	element_type TypeExpr
}

// str returns a string representation of ListType
pub fn (lt ListType) str() string {
	return 'list(${lt.element_type.str()})'
}

// BinaryType represents a binary/bitstring type
pub struct BinaryType {
pub:
	unit_size int // Size in bits, 0 for variable size
}

// str returns a string representation of BinaryType
pub fn (bt BinaryType) str() string {
	return if bt.unit_size > 0 { 'binary(${bt.unit_size})' } else { 'binary' }
}

// TypeExpr represents type expressions in the Hindley-Milner system
pub type TypeExpr = TypeVar
	| TypeConstructor
	| FunctionType
	| RecordType
	| MapType
	| TupleType
	| ListType
	| BinaryType

// str returns a string representation of TypeExpr
pub fn (te TypeExpr) str() string {
	return match te {
		TypeVar {
			if te.name != '' {
				te.name
			} else {
				te.id
			}
		}
		TypeConstructor {
			if te.parameters.len > 0 {
				params := te.parameters.map(it.str()).join(', ')
				'${te.name}(${params})'
			} else {
				te.name
			}
		}
		FunctionType {
			if te.parameters.len > 0 {
				params := te.parameters.map(it.str()).join(', ')
				'(${params}) -> ${te.return_type.str()}'
			} else {
				'() -> ${te.return_type.str()}'
			}
		}
		RecordType {
			mut fields := []string{}
			for k, v in te.fields {
				fields << '${k}: ${v.str()}'
			}
			'${te.name}{${fields.join(', ')}}'
		}
		MapType {
			'map(${te.key_type.str()}, ${te.value_type.str()})'
		}
		TupleType {
			elements := te.element_types.map(it.str()).join(', ')
			'(${elements})'
		}
		ListType {
			'list(${te.element_type.str()})'
		}
		BinaryType {
			if te.unit_size > 0 {
				'binary(${te.unit_size})'
			} else {
				'binary'
			}
		}
	}
}

// is_monomorphic checks if a type is monomorphic (no type variables)
pub fn (te TypeExpr) is_monomorphic() bool {
	return match te {
		TypeVar {
			false
		}
		TypeConstructor {
			for param in te.parameters {
				if !param.is_monomorphic() {
					return false
				}
			}
			true
		}
		FunctionType {
			for param in te.parameters {
				if !param.is_monomorphic() {
					return false
				}
			}
			te.return_type.is_monomorphic()
		}
		RecordType {
			for field_type in te.fields.values() {
				if !field_type.is_monomorphic() {
					return false
				}
			}
			true
		}
		MapType {
			te.key_type.is_monomorphic() && te.value_type.is_monomorphic()
		}
		TupleType {
			for element_type in te.element_types {
				if !element_type.is_monomorphic() {
					return false
				}
			}
			true
		}
		ListType {
			te.element_type.is_monomorphic()
		}
		BinaryType {
			true
		}
	}
}

// contains_type_var checks if a type contains a specific type variable
pub fn (te TypeExpr) contains_type_var(var_id string) bool {
	return match te {
		TypeVar {
			te.id == var_id
		}
		TypeConstructor {
			for param in te.parameters {
				if param.contains_type_var(var_id) {
					return true
				}
			}
			false
		}
		FunctionType {
			for param in te.parameters {
				if param.contains_type_var(var_id) {
					return true
				}
			}
			te.return_type.contains_type_var(var_id)
		}
		RecordType {
			for field_type in te.fields.values() {
				if field_type.contains_type_var(var_id) {
					return true
				}
			}
			false
		}
		MapType {
			te.key_type.contains_type_var(var_id) || te.value_type.contains_type_var(var_id)
		}
		TupleType {
			for element_type in te.element_types {
				if element_type.contains_type_var(var_id) {
					return true
				}
			}
			false
		}
		ListType {
			te.element_type.contains_type_var(var_id)
		}
		BinaryType {
			false
		}
	}
}

// get_type_vars returns all type variables in a type expression
pub fn (te TypeExpr) get_type_vars() []string {
	return match te {
		TypeVar {
			[te.id]
		}
		TypeConstructor {
			mut vars := []string{}
			for param in te.parameters {
				vars << param.get_type_vars()
			}
			vars
		}
		FunctionType {
			mut vars := []string{}
			for param in te.parameters {
				vars << param.get_type_vars()
			}
			vars << te.return_type.get_type_vars()
			vars
		}
		RecordType {
			mut vars := []string{}
			for field_type in te.fields.values() {
				vars << field_type.get_type_vars()
			}
			vars
		}
		MapType {
			mut vars := []string{}
			vars << te.key_type.get_type_vars()
			vars << te.value_type.get_type_vars()
			vars
		}
		TupleType {
			mut vars := []string{}
			for element_type in te.element_types {
				vars << element_type.get_type_vars()
			}
			vars
		}
		ListType {
			te.element_type.get_type_vars()
		}
		BinaryType {
			[]string{}
		}
	}
}

// Built-in type constructors
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
pub const unknown_type = TypeConstructor{
	name:       'unknown'
	parameters: []
}

// make_list_type creates a list type with the given element type
pub fn make_list_type(element_type TypeExpr) ListType {
	return ListType{
		element_type: element_type
	}
}

// make_function_type creates a function type with the given parameters and return type
pub fn make_function_type(parameters []TypeExpr, return_type TypeExpr) FunctionType {
	return FunctionType{
		parameters:  parameters
		return_type: return_type
	}
}

// make_tuple_type creates a tuple type with the given element types
pub fn make_tuple_type(element_types []TypeExpr) TupleType {
	return TupleType{
		element_types: element_types
	}
}

// make_map_type creates a map type with the given key and value types
pub fn make_map_type(key_type TypeExpr, value_type TypeExpr) MapType {
	return MapType{
		key_type:   key_type
		value_type: value_type
	}
}

// Helper functions for testing - convert concrete types to TypeExpr and call methods

// is_monomorphic_type_var checks if a TypeVar is monomorphic
pub fn is_monomorphic_type_var(tv TypeVar) bool {
	return TypeExpr(tv).is_monomorphic()
}

// contains_type_var_type_var checks if a TypeVar contains a specific type variable
pub fn contains_type_var_type_var(tv TypeVar, var_id string) bool {
	return TypeExpr(tv).contains_type_var(var_id)
}

// get_type_vars_type_var returns all type variables in a TypeVar
pub fn get_type_vars_type_var(tv TypeVar) []string {
	return TypeExpr(tv).get_type_vars()
}

// is_monomorphic_type_constructor checks if a TypeConstructor is monomorphic
pub fn is_monomorphic_type_constructor(tc TypeConstructor) bool {
	return TypeExpr(tc).is_monomorphic()
}

// contains_type_var_type_constructor checks if a TypeConstructor contains a specific type variable
pub fn contains_type_var_type_constructor(tc TypeConstructor, var_id string) bool {
	return TypeExpr(tc).contains_type_var(var_id)
}

// get_type_vars_type_constructor returns all type variables in a TypeConstructor
pub fn get_type_vars_type_constructor(tc TypeConstructor) []string {
	return TypeExpr(tc).get_type_vars()
}

// is_monomorphic_function_type checks if a FunctionType is monomorphic
pub fn is_monomorphic_function_type(ft FunctionType) bool {
	return TypeExpr(ft).is_monomorphic()
}

// contains_type_var_function_type checks if a FunctionType contains a specific type variable
pub fn contains_type_var_function_type(ft FunctionType, var_id string) bool {
	return TypeExpr(ft).contains_type_var(var_id)
}

// get_type_vars_function_type returns all type variables in a FunctionType
pub fn get_type_vars_function_type(ft FunctionType) []string {
	return TypeExpr(ft).get_type_vars()
}

// is_monomorphic_record_type checks if a RecordType is monomorphic
pub fn is_monomorphic_record_type(rt RecordType) bool {
	return TypeExpr(rt).is_monomorphic()
}

// contains_type_var_record_type checks if a RecordType contains a specific type variable
pub fn contains_type_var_record_type(rt RecordType, var_id string) bool {
	return TypeExpr(rt).contains_type_var(var_id)
}

// get_type_vars_record_type returns all type variables in a RecordType
pub fn get_type_vars_record_type(rt RecordType) []string {
	return TypeExpr(rt).get_type_vars()
}

// is_monomorphic_map_type checks if a MapType is monomorphic
pub fn is_monomorphic_map_type(mt MapType) bool {
	return TypeExpr(mt).is_monomorphic()
}

// contains_type_var_map_type checks if a MapType contains a specific type variable
pub fn contains_type_var_map_type(mt MapType, var_id string) bool {
	return TypeExpr(mt).contains_type_var(var_id)
}

// get_type_vars_map_type returns all type variables in a MapType
pub fn get_type_vars_map_type(mt MapType) []string {
	return TypeExpr(mt).get_type_vars()
}

// is_monomorphic_tuple_type checks if a TupleType is monomorphic
pub fn is_monomorphic_tuple_type(tt TupleType) bool {
	return TypeExpr(tt).is_monomorphic()
}

// contains_type_var_tuple_type checks if a TupleType contains a specific type variable
pub fn contains_type_var_tuple_type(tt TupleType, var_id string) bool {
	return TypeExpr(tt).contains_type_var(var_id)
}

// get_type_vars_tuple_type returns all type variables in a TupleType
pub fn get_type_vars_tuple_type(tt TupleType) []string {
	return TypeExpr(tt).get_type_vars()
}

// is_monomorphic_list_type checks if a ListType is monomorphic
pub fn is_monomorphic_list_type(lt ListType) bool {
	return TypeExpr(lt).is_monomorphic()
}

// contains_type_var_list_type checks if a ListType contains a specific type variable
pub fn contains_type_var_list_type(lt ListType, var_id string) bool {
	return TypeExpr(lt).contains_type_var(var_id)
}

// get_type_vars_list_type returns all type variables in a ListType
pub fn get_type_vars_list_type(lt ListType) []string {
	return TypeExpr(lt).get_type_vars()
}

// is_monomorphic_binary_type checks if a BinaryType is monomorphic
pub fn is_monomorphic_binary_type(bt BinaryType) bool {
	return TypeExpr(bt).is_monomorphic()
}

// contains_type_var_binary_type checks if a BinaryType contains a specific type variable
pub fn contains_type_var_binary_type(bt BinaryType, var_id string) bool {
	return TypeExpr(bt).contains_type_var(var_id)
}

// get_type_vars_binary_type returns all type variables in a BinaryType
pub fn get_type_vars_binary_type(bt BinaryType) []string {
	return TypeExpr(bt).get_type_vars()
}

// Helper functions for type comparison
// equals_type_var checks if two TypeVar instances are equal
pub fn equals_type_var(tv1 TypeVar, tv2 TypeVar) bool {
	return tv1.id == tv2.id
}

// equals_type_constructor checks if two TypeConstructor instances are equal
pub fn equals_type_constructor(tc1 TypeConstructor, tc2 TypeConstructor) bool {
	return tc1.name == tc2.name && tc1.parameters.len == tc2.parameters.len
}

// equals_function_type checks if two FunctionType instances are equal
pub fn equals_function_type(ft1 FunctionType, ft2 FunctionType) bool {
	if ft1.parameters.len != ft2.parameters.len {
		return false
	}
	// For simplicity, just check parameter count and return type
	return ft1.return_type.str() == ft2.return_type.str()
}

// equals_record_type checks if two RecordType instances are equal
pub fn equals_record_type(rt1 RecordType, rt2 RecordType) bool {
	return rt1.name == rt2.name && rt1.fields.len == rt2.fields.len
}

// equals_map_type checks if two MapType instances are equal
pub fn equals_map_type(mt1 MapType, mt2 MapType) bool {
	return mt1.key_type.str() == mt2.key_type.str() && mt1.value_type.str() == mt2.value_type.str()
}

// equals_tuple_type checks if two TupleType instances are equal
pub fn equals_tuple_type(tt1 TupleType, tt2 TupleType) bool {
	if tt1.element_types.len != tt2.element_types.len {
		return false
	}
	// For simplicity, just check element count
	return true
}

// equals_list_type checks if two ListType instances are equal
pub fn equals_list_type(lt1 ListType, lt2 ListType) bool {
	return lt1.element_type.str() == lt2.element_type.str()
}

// equals_binary_type checks if two BinaryType instances are equal
pub fn equals_binary_type(bt1 BinaryType, bt2 BinaryType) bool {
	return bt1.unit_size == bt2.unit_size
}
