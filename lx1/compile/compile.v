module compile

import os
import parser
import analysis
import generator
import ast
import errors

pub fn compile_file(file_path string, show_type_table bool, show_nodes bool) {
	if !os.exists(file_path) {
		eprintln('Error: File "${file_path}" not found')
		exit(1)
	}

	content := os.read_file(file_path) or {
		eprintln('Error reading file "${file_path}": ${err}')
		exit(1)
	}

	if content.trim_space().len == 0 {
		eprintln('Error: File "${file_path}" is empty')
		exit(1)
	}

	module_name := os.file_name(file_path).all_before_last('.')

	result := compile_string_with_modname(content, file_path, module_name, show_type_table,
		show_nodes) or {
		eprintln('Compilation failed: ${err}')
		exit(1)
	}
	dir := os.dir(file_path)
	erl_file := os.join_path(dir, module_name + '.erl')
	os.write_file(erl_file, result) or {
		eprintln('Failed to write ${erl_file}: ${err}')
		exit(1)
	}
}

pub fn compile_string(code string, file_path string, show_type_table bool, show_nodes bool) !string {
	module_name := os.file_name(file_path).all_before_last('.')
	return compile_string_with_modname(code, file_path, module_name, show_type_table,
		show_nodes)
}

pub fn compile_string_with_modname(code string, file_path string, module_name string, show_type_table bool, show_nodes bool) !string {
	mut p := parser.new_parser(code, file_path)
	ast_node := p.parse_with_modname(module_name) or {
		parser_errors := p.get_errors()
		if parser_errors.len > 0 {
			file_lines := os.read_file(file_path) or { '' }
			lines := file_lines.split('\n')
			mut error_msg := ''
			for e in parser_errors {
				error_msg += errors.format_error_detailed(e, lines) + '\n'
			}
			return error(error_msg)
		}
		return error('Parse error: ${err}')
	}

	parser_errors := p.get_errors()
	if parser_errors.len > 0 {
		file_lines := os.read_file(file_path) or { '' }
		lines := file_lines.split('\n')
		mut error_msg := ''
		for e in parser_errors {
			error_msg += errors.format_error_detailed(e, lines) + '\n'
		}
		return error(error_msg)
	}

	if show_nodes {
		println('=== AST NODES ===')
		print_ast_nodes(ast_node, 0)
		println('=================')
	}

	mut analyzer := analysis.new_analyzer()
	analyzed_ast := analyzer.analyze(ast_node) or {
		analysis_errors := analyzer.get_errors()
		if analysis_errors.len > 0 {
			mut error_msg := ''
			for e in analysis_errors {
				error_msg += errors.format_error(e) + '\n'
			}
			return error(error_msg)
		}
		return error('Analysis error: ${err}')
	}
	analysis_errors := analyzer.get_errors()
	if analysis_errors.len > 0 {
		file_lines := os.read_file(file_path) or { '' }
		lines := file_lines.split('\n')
		mut error_msg := ''
		for e in analysis_errors {
			error_msg += errors.format_error_detailed(e, lines) + '\n'
		}
		return error(error_msg)
	}

	if show_type_table {
		analyzer.get_type_table().debug_print()
	}

	mut gen := generator.new_generator()
	erlang_code := gen.generate_with_types(analyzed_ast, analyzer.get_type_table()) or {
		errs := gen.get_errors()
		if errs.len > 0 {
			return error('Generation errors:\n${errs.join('\n')}')
		}
		return error('Generation error: ${err}')
	}

	generation_errors := gen.get_errors()
	if generation_errors.len > 0 {
		return error('Generation errors:\n${generation_errors.join('\n')}')
	}
	println('Compiled ${file_path}')
	return erlang_code
}

fn print_ast_nodes(node ast.Node, indent int) {
	pad := '  '.repeat(indent)
	println('${pad}- Node(id=${node.id}, kind=${node.kind}, value="${node.value}")')
	for child in node.children {
		print_ast_nodes(child, indent + 1)
	}
}
