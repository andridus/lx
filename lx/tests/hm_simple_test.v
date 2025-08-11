module main

import ast
import analysis

fn test_polymorphic_identity() {
	// Test identity function type inference
	arg_types := [ast.Type{
		name:   'integer'
		params: []
	}]
	result_type := analysis.infer_polymorphic_type('identity', arg_types)

	assert result_type.name == 'function'
	assert result_type.params.len == 2
	assert result_type.params[0].name == 'integer'
	assert result_type.params[1].name == 'integer'

	println('✅ identity: ${result_type.str()}')
}

fn test_polymorphic_map() {
	// Test map function type inference
	func_type := ast.Type{
		name:   'function'
		params: [ast.Type{
			name:   'integer'
			params: []
		}, ast.Type{
			name:   'string'
			params: []
		}]
	}
	list_type := ast.Type{
		name:   'list'
		params: [ast.Type{
			name:   'integer'
			params: []
		}]
	}
	arg_types := [func_type, list_type]

	result_type := analysis.infer_polymorphic_type('map', arg_types)

	assert result_type.name == 'function'
	assert result_type.params.len == 3
	assert result_type.params[2].name == 'list'

	println('✅ map: ${result_type.str()}')
}

fn test_polymorphic_head() {
	// Test head function type inference
	list_type := ast.Type{
		name:   'list'
		params: [ast.Type{
			name:   'string'
			params: []
		}]
	}
	arg_types := [list_type]

	result_type := analysis.infer_polymorphic_type('head', arg_types)

	assert result_type.name == 'string'

	println('✅ head: ${result_type.str()}')
}

fn test_polymorphic_length() {
	// Test length function type inference
	list_type := ast.Type{
		name:   'list'
		params: [ast.Type{
			name:   'integer'
			params: []
		}]
	}
	arg_types := [list_type]

	result_type := analysis.infer_polymorphic_type('length', arg_types)

	assert result_type.name == 'integer'

	println('✅ length: ${result_type.str()}')
}

fn test_polymorphic_compose() {
	// Test compose function type inference
	func1 := ast.Type{
		name:   'function'
		params: [ast.Type{
			name:   'integer'
			params: []
		}, ast.Type{
			name:   'string'
			params: []
		}]
	}
	func2 := ast.Type{
		name:   'function'
		params: [ast.Type{
			name:   'float'
			params: []
		}, ast.Type{
			name:   'integer'
			params: []
		}]
	}
	arg_types := [func1, func2]

	result_type := analysis.infer_polymorphic_type('compose', arg_types)

	assert result_type.name == 'function'
	assert result_type.params.len == 2

	println('✅ compose: ${result_type.str()}')
}

fn test_is_polymorphic_type() {
	// Test polymorphic type detection
	simple_type := ast.Type{
		name:   'integer'
		params: []
	}
	assert !analysis.is_polymorphic_type(simple_type)

	polymorphic_type := ast.Type{
		name:   'list'
		params: [ast.Type{
			name:   'T1'
			params: []
		}]
	}
	assert analysis.is_polymorphic_type(polymorphic_type)

	type_var := ast.Type{
		name:   'T1'
		params: []
	}
	assert analysis.is_polymorphic_type(type_var)

	println('✅ is_polymorphic_type: works correctly')
}

fn test_get_type_variables() {
	// Test type variable extraction
	complex_type := ast.Type{
		name:   'function'
		params: [
			ast.Type{
				name:   'T1'
				params: []
			},
			ast.Type{
				name:   'list'
				params: [ast.Type{
					name:   'T2'
					params: []
				}]
			},
			ast.Type{
				name:   'T3'
				params: []
			},
		]
	}

	vars := analysis.get_type_variables(complex_type)
	assert vars.len == 3
	assert 'T1' in vars
	assert 'T2' in vars
	assert 'T3' in vars

	println('✅ get_type_variables: found ${vars.len} variables')
}

fn test_unify_types() {
	// Test type unification
	t1 := ast.Type{
		name:   'integer'
		params: []
	}
	t2 := ast.Type{
		name:   'integer'
		params: []
	}
	assert analysis.unify_types(t1, t2)

	t3 := ast.Type{
		name:   'string'
		params: []
	}
	assert !analysis.unify_types(t1, t3)

	t4 := ast.Type{
		name:   'T1'
		params: []
	}
	assert analysis.unify_types(t1, t4) // Type variable can unify with anything

	t5 := ast.Type{
		name:   'any'
		params: []
	}
	assert analysis.unify_types(t1, t5) // any can unify with anything

	println('✅ unify_types: works correctly')
}

fn test_generate_polymorphic_spec() {
	// Test Erlang spec generation
	func_name := 'identity'
	func_type := ast.Type{
		name:   'function'
		params: [ast.Type{
			name:   'integer'
			params: []
		}, ast.Type{
			name:   'integer'
			params: []
		}]
	}

	spec := analysis.generate_polymorphic_spec(func_name, func_type)
	expected := '(integer()) -> integer()'
	assert spec == expected

	println('✅ generate_polymorphic_spec: ${spec}')
}

fn test_generate_complex_spec() {
	// Test complex Erlang spec generation
	func_name := 'map'
	func_type := ast.Type{
		name:   'function'
		params: [
			ast.Type{
				name:   'function'
				params: [ast.Type{
					name:   'integer'
					params: []
				}, ast.Type{
					name:   'string'
					params: []
				}]
			},
			ast.Type{
				name:   'list'
				params: [ast.Type{
					name:   'integer'
					params: []
				}]
			},
			ast.Type{
				name:   'list'
				params: [ast.Type{
					name:   'T1'
					params: []
				}]
			},
		]
	}

	spec := analysis.generate_polymorphic_spec(func_name, func_type)
	println('✅ generate_complex_spec: ${spec}')
}

fn test_list_spec_generation() {
	// Test list type spec generation
	list_type := ast.Type{
		name:   'list'
		params: [ast.Type{
			name:   'integer'
			params: []
		}]
	}

	spec := analysis.convert_type_to_erlang(list_type)
	assert spec == '[integer()]'

	println('✅ list_spec_generation: ${spec}')
}

fn test_map_spec_generation() {
	// Test map type spec generation
	map_type := ast.Type{
		name:   'map'
		params: [ast.Type{
			name:   'string'
			params: []
		}, ast.Type{
			name:   'integer'
			params: []
		}]
	}

	spec := analysis.convert_type_to_erlang(map_type)
	assert spec == '#{binary() => integer()}'

	println('✅ map_spec_generation: ${spec}')
}

fn test_tuple_spec_generation() {
	// Test tuple type spec generation
	tuple_type := ast.Type{
		name:   'tuple'
		params: [ast.Type{
			name:   'integer'
			params: []
		}, ast.Type{
			name:   'string'
			params: []
		}, ast.Type{
			name:   'boolean'
			params: []
		}]
	}

	spec := analysis.convert_type_to_erlang(tuple_type)
	assert spec == '{integer(), binary(), boolean()}'

	println('✅ tuple_spec_generation: ${spec}')
}

fn test_function_spec_generation() {
	// Test function type spec generation
	func_type := ast.Type{
		name:   'function'
		params: [ast.Type{
			name:   'integer'
			params: []
		}, ast.Type{
			name:   'string'
			params: []
		}, ast.Type{
			name:   'boolean'
			params: []
		}]
	}

	spec := analysis.convert_type_to_erlang(func_type)
	assert spec == '(integer(), binary()) -> boolean()'

	println('✅ function_spec_generation: ${spec}')
}

fn test_type_variable_spec_generation() {
	// Test type variable spec generation
	type_var := ast.Type{
		name:   'T1'
		params: []
	}

	spec := analysis.convert_type_to_erlang(type_var)
	assert spec == 'any()'

	println('✅ type_variable_spec_generation: ${spec}')
}

fn test_complex_polymorphic_function() {
	// Test a complex polymorphic function
	func_name := 'complex_function'

	// Function type: (A, B, (A -> B)) -> B
	param_a := ast.Type{
		name:   'T1'
		params: []
	}
	param_b := ast.Type{
		name:   'T2'
		params: []
	}
	func_param := ast.Type{
		name:   'function'
		params: [param_a, param_b]
	}

	func_type := ast.Type{
		name:   'function'
		params: [param_a, param_b, func_param, param_b]
	}

	spec := analysis.generate_polymorphic_spec(func_name, func_type)
	println('✅ complex_polymorphic_function: ${spec}')
}

fn test_hm_type_system_errors() {
	// Test error handling
	mut hm_system := analysis.new_hm_type_system()

	// Add some errors
	hm_system.error('Type mismatch in function call', ast.Position{})
	hm_system.error('Undefined variable: x', ast.Position{})

	errors := hm_system.get_errors()
	assert errors.len == 2

	println('✅ hm_type_system_errors: ${errors.len} errors captured')
}

fn test_integration_scenario() {
	// Test a complete integration scenario
	println('\n🔍 Testing Integration Scenario:')

	// 1. Define polymorphic functions
	functions := [
		'identity',
		'map',
		'head',
		'length',
		'compose',
	]

	// 2. Test each function
	for func_name in functions {
		// Create some argument types
		arg_types := match func_name {
			'identity' {
				[ast.Type{
					name:   'integer'
					params: []
				}]
			}
			'map' {
				[
					ast.Type{
						name:   'function'
						params: [
							ast.Type{
								name:   'integer'
								params: []
							},
							ast.Type{
								name:   'string'
								params: []
							},
						]
					},
					ast.Type{
						name:   'list'
						params: [
							ast.Type{
								name:   'integer'
								params: []
							},
						]
					},
				]
			}
			'head' {
				[
					ast.Type{
						name:   'list'
						params: [ast.Type{
							name:   'string'
							params: []
						}]
					},
				]
			}
			'length' {
				[
					ast.Type{
						name:   'list'
						params: [
							ast.Type{
								name:   'boolean'
								params: []
							},
						]
					},
				]
			}
			'compose' {
				[
					ast.Type{
						name:   'function'
						params: [
							ast.Type{
								name:   'integer'
								params: []
							},
							ast.Type{
								name:   'string'
								params: []
							},
						]
					},
					ast.Type{
						name:   'function'
						params: [
							ast.Type{
								name:   'float'
								params: []
							},
							ast.Type{
								name:   'integer'
								params: []
							},
						]
					},
				]
			}
			else {
				[ast.Type{
					name:   'integer'
					params: []
				}]
			}
		}

		// Infer type
		inferred_type := analysis.infer_polymorphic_type(func_name, arg_types)

		// Generate spec
		spec := analysis.generate_polymorphic_spec(func_name, inferred_type)

		println('  ${func_name}: ${inferred_type.str()} -> ${spec}')
	}

	println('✅ Integration scenario completed successfully')
}

fn main() {
	println('🧪 Running HM Type System Tests...\n')

	test_polymorphic_identity()
	test_polymorphic_map()
	test_polymorphic_head()
	test_polymorphic_length()
	test_polymorphic_compose()
	test_is_polymorphic_type()
	test_get_type_variables()
	test_unify_types()
	test_generate_polymorphic_spec()
	test_generate_complex_spec()
	test_list_spec_generation()
	test_map_spec_generation()
	test_tuple_spec_generation()
	test_function_spec_generation()
	test_type_variable_spec_generation()
	test_complex_polymorphic_function()
	test_hm_type_system_errors()
	test_integration_scenario()

	println('\n🎉 All HM Type System tests passed!')
}
