module erlang

import ast

// generate_pattern generates code for patterns
pub fn (mut gen ErlangGenerator) generate_pattern(pattern ast.Pattern) string {
	match pattern {
		ast.WildcardPattern {
			return '_'
		}
		ast.VarPattern {
			return gen.capitalize_variable(pattern.name)
		}
		ast.LiteralPattern {
			return gen.generate_literal(pattern.value)
		}
		ast.AtomPattern {
			return pattern.value
		}
		ast.ListConsPattern {
			head := gen.generate_pattern(pattern.head)
			tail := gen.generate_pattern(pattern.tail)
			return '[${head} | ${tail}]'
		}
		ast.ListEmptyPattern {
			return '[]'
		}
		ast.ListLiteralPattern {
			elements := pattern.elements.map(gen.generate_pattern(it))
			return '[${elements.join(', ')}]'
		}
		ast.TuplePattern {
			elements := pattern.elements.map(gen.generate_pattern(it))
			return '{${elements.join(', ')}}'
		}
		ast.MapPattern {
			mut entries := []string{}
			for entry in pattern.entries {
				key := gen.generate_pattern(entry.key)
				value := gen.generate_pattern(entry.value)
				entries << '${key} => ${value}'
			}

			assign_var := pattern.assign_variable or { '' }
			if assign_var != '' {
				capitalized_var := gen.capitalize_variable(assign_var)
				return '#{${entries.join(', ')}} = ${capitalized_var}'
			}

			return '#{${entries.join(', ')}}'
		}
		ast.RecordPattern {
			mut fields := []string{}
			for field in pattern.fields {
				value := gen.generate_pattern(field.pattern)
				fields << '${field.name} = ${value}'
			}

			assign_var := pattern.assign_variable or { '' }
			if assign_var != '' {
				capitalized_var := gen.capitalize_variable(assign_var)
				return '#${pattern.name.to_lower()}{${fields.join(', ')}} = ${capitalized_var}'
			}

			return '#${pattern.name.to_lower()}{${fields.join(', ')}}'
		}
		ast.BinaryPattern {
			mut segments := []string{}
			for segment in pattern.segments {
				mut seg_code := ''
				if segment.value is ast.VariableExpr {
					var_name := (segment.value as ast.VariableExpr).name
					seg_code += gen.capitalize_variable(var_name)
				} else {
					seg_code += gen.generate_expression(segment.value)
				}
				if segment.size != none {
					seg_code += ':' + gen.generate_expression(segment.size)
				}
				if segment.options.len > 0 {
					seg_code += '/' + segment.options.join("-")
				}
				segments << seg_code
			}
			return '<<${segments.join(', ')}>>'
		}
	}
}

// generate_pattern_with_binding generates code for patterns and binds variables to scope
pub fn (mut gen ErlangGenerator) generate_pattern_with_binding(pattern ast.Pattern) string {
	match pattern {
		ast.WildcardPattern {
			return '_'
		}
		ast.VarPattern {
			// Bind the variable to the scope e retorne o nome capitalizado
			hashed := gen.bind_variable(pattern.name, true)
			return hashed
		}
		ast.LiteralPattern {
			return gen.generate_literal(pattern.value)
		}
		ast.AtomPattern {
			return pattern.value
		}
		ast.ListConsPattern {
			head := gen.generate_pattern_with_binding(pattern.head)
			tail := gen.generate_pattern_with_binding(pattern.tail)
			return '[${head} | ${tail}]'
		}
		ast.ListEmptyPattern {
			return '[]'
		}
		ast.ListLiteralPattern {
			elements := pattern.elements.map(gen.generate_pattern_with_binding(it))
			return '[${elements.join(', ')}]'
		}
		ast.TuplePattern {
			elements := pattern.elements.map(gen.generate_pattern_with_binding(it))
			return '{${elements.join(', ')}}'
		}
		ast.MapPattern {
			mut entries := []string{}
			for entry in pattern.entries {
				key := gen.generate_pattern_with_binding(entry.key)
				value := gen.generate_pattern_with_binding(entry.value)
				entries << '${key} => ${value}'
			}
			return '#{${entries.join(', ')}}'
		}
		ast.RecordPattern {
			mut fields := []string{}
			for field in pattern.fields {
				value := gen.generate_pattern_with_binding(field.pattern)
				fields << '${field.name} = ${value}'
			}
			assign_var := pattern.assign_variable or { '' }
			if assign_var != '' {
				capitalized_var := gen.bind_variable(assign_var, true)
				return '#${pattern.name.to_lower()}{${fields.join(', ')}} = ${capitalized_var}'
			}
			return '#${pattern.name.to_lower()}{${fields.join(', ')}}'
		}
		ast.BinaryPattern {
			mut segments := []string{}
			for segment in pattern.segments {
				mut seg_code := ''
				if segment.value is ast.VariableExpr {
					var_name := (segment.value as ast.VariableExpr).name
					seg_code += gen.capitalize_variable(var_name)
					if segment.size != none {
						seg_code += ':' + gen.generate_expression(segment.size)
					}
				} else {
					seg_code += gen.generate_expression(segment.value)
				if segment.size != none {
					seg_code += ':' + gen.generate_expression(segment.size)
					}
				}
				if segment.options.len > 0 {
					seg_code += '/' + segment.options.join("-")
				}
				segments << seg_code
			}
			return '<<${segments.join(', ')}>>'
		}
	}
}