module erlang

import ast

// generate_pattern generates code for patterns
pub fn (gen ErlangGenerator) generate_pattern(pattern ast.Pattern) string {
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
			return '#{${entries.join(', ')}}'
		}
		ast.RecordPattern {
			mut fields := []string{}
			for field in pattern.fields {
				value := gen.generate_pattern(field.pattern)
				fields << '${field.name}: ${value}'
			}
			return '#${pattern.name}{${fields.join(', ')}}'
		}
		ast.BinaryPattern {
			mut segments := []string{}
			for segment in pattern.segments {
				segments << '${segment.size}:${segment.unit}'
			}
			return '<<${segments.join(', ')}>>'
		}
	}
}
