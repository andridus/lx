module erlang

// Erlang code formatting utilities

// format_indent adds indentation to a string
pub fn format_indent(code string, indent_level int) string {
	if indent_level <= 0 {
		return code
	}

	indent := '    '.repeat(indent_level)
	lines := code.split('\n')
	mut formatted_lines := []string{}

	for line in lines {
		if line.trim_space().len > 0 {
			formatted_lines << '${indent}${line}'
		} else {
			formatted_lines << line
		}
	}

	return formatted_lines.join('\n')
}

// format_function_clause formats a function clause with proper indentation
pub fn format_function_clause(name string, parameters []string, guard string, body string) string {
	header := '${name}(${parameters.join(', ')})${guard} ->'
	formatted_body := format_indent(body, 1)
	return '${header}\n${formatted_body}'
}

// format_case_clause formats a case clause with proper indentation
pub fn format_case_clause(pattern string, guard string, body string) string {
	header := '${pattern}${guard} ->'
	formatted_body := format_indent(body, 1)
	return '${header}\n${formatted_body}'
}

// format_receive_clause formats a receive clause with proper indentation
pub fn format_receive_clause(pattern string, guard string, body string) string {
	header := '${pattern}${guard} ->'
	formatted_body := format_indent(body, 1)
	return '${header}\n${formatted_body}'
}

// format_list formats a list with proper spacing
pub fn format_list(elements []string) string {
	if elements.len == 0 {
		return '[]'
	}
	return '[${elements.join(', ')}]'
}

// format_tuple formats a tuple with proper spacing
pub fn format_tuple(elements []string) string {
	if elements.len == 0 {
		return '{}'
	}
	return '{${elements.join(', ')}}'
}

// format_map formats a map with proper spacing
pub fn format_map(entries []string) string {
	if entries.len == 0 {
		return '#{}'
	}
	return '#{${entries.join(', ')}}'
}

// format_record formats a record with proper spacing
pub fn format_record(name string, fields []string) string {
	if fields.len == 0 {
		return '#${name}{}'
	}
	return '#${name}{${fields.join(', ')}}'
}
