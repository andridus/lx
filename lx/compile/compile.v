module compile

import os
import parser
import analysis
import generator
import ast
import errors

// Compilation result for multi-file generation
pub struct CompilationResult {
pub mut:
	main_module     string
	files           map[string]string // filename -> content
	app_src_content string
	hrl_content     string
}

pub fn compile_file(file_path string) {
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
	dir := os.dir(file_path)

	// Check if this needs multi-file compilation
	if needs_multi_file_compilation(content) {
		result := compile_multi_file(content, file_path, module_name) or {
			eprintln('Multi-file compilation failed: ${err}')
			exit(1)
		}

		// Write all generated files into the same dir as input (expected to be _build/apps/<app>/src)
		for filename, file_content in result.files {
			output_path := os.join_path(dir, filename)
			os.write_file(output_path, file_content) or {
				eprintln('Failed to write ${output_path}: ${err}')
				exit(1)
			}
			println('Generated ${output_path}')
		}

		// Write .app.src if present
		if result.app_src_content.len > 0 {
			app_src_path := os.join_path(dir, '${module_name}.app.src')
			os.write_file(app_src_path, result.app_src_content) or {
				eprintln('Failed to write ${app_src_path}: ${err}')
				exit(1)
			}
			println('Generated ${app_src_path}')
		}

		// Write .hrl if present
		if result.hrl_content.len > 0 {
			hrl_path := os.join_path(os.dir(dir), 'include')
			os.mkdir_all(hrl_path) or {}
			hrl_file := os.join_path(hrl_path, '${module_name}.hrl')
			os.write_file(hrl_file, result.hrl_content) or {
				eprintln('Failed to write ${hrl_file}: ${err}')
				exit(1)
			}
			println('Generated ${hrl_file}')
		}
	} else {
		// Single file compilation (legacy)
		result := compile_string_with_modname(content, file_path, module_name) or {
			eprintln('Compilation failed: ${err}')
			exit(1)
		}
		erl_file := os.join_path(dir, module_name + '.erl')
		os.write_file(erl_file, result) or {
			eprintln('Failed to write ${erl_file}: ${err}')
			exit(1)
		}
	}
}

// Check if source needs multi-file compilation (has application, supervisor, worker)
fn needs_multi_file_compilation(code string) bool {
	return code.contains('application {') || code.contains('supervisor ')
		|| code.contains('worker ')
}

// Multi-file compilation: generates separate modules for each construct
pub fn compile_multi_file(code string, file_path string, base_module_name string) !CompilationResult {
	mut directives_table := parser.new_directives_table()
	mut p := parser.new_parser(code, file_path, directives_table)
	ast_node := p.parse_with_modname(base_module_name) or {
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

	mut analyzer := analysis.new_analyzer()
	analyzed_ast := analyzer.analyze(ast_node) or {
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

	// Extract different constructs from AST
	modules := extract_modules_from_ast(analyzed_ast, base_module_name)!

	mut result := CompilationResult{
		main_module: base_module_name
		files:       map[string]string{}
	}

	// Generate each module
	for module_info in modules {
		mut gen := generator.new_generator(directives_table)
		erlang_code := gen.generate_with_types(module_info.ast_node, analyzer.get_type_table()) or {
			errs := gen.get_errors()
			if errs.len > 0 {
				return error('Generation errors for ${module_info.name}:\n${errs.join('\n')}')
			}
			return error('Generation error for ${module_info.name}: ${err}')
		}

		generation_errors := gen.get_errors()
		if generation_errors.len > 0 {
			return error('Generation errors for ${module_info.name}:\n${generation_errors.join('\n')}')
		}

		result.files[module_info.filename] = erlang_code
		println('Compiled module ${module_info.name}')
	}

	// Ensure an application behaviour module exists so the app can start under rebar3 shell
	app_module_name := base_module_name + '_app'
	// Choose a root supervisor if any was defined (first *_sup module)
	mut root_sup := ''
	for module_info in modules {
		if module_info.name.ends_with('_sup') {
			root_sup = module_info.name
			break
		}
	}
	app_module_filename := app_module_name + '.erl'
	if app_module_filename !in result.files {
		result.files[app_module_filename] = generate_application_behaviour_module(app_module_name,
			root_sup)
	}

	// Generate .app.src and .hrl if needed
	result.app_src_content = generate_app_src(analyzed_ast, base_module_name)!
	result.hrl_content = generate_hrl(analyzed_ast, base_module_name)!

	return result
}

// Fallback multi-file compilation without analysis (safer for OTP scaffolds)
pub fn compile_multi_file_no_analyze(code string, file_path string, base_module_name string) !CompilationResult {
	mut directives_table := parser.new_directives_table()
	mut p := parser.new_parser(code, file_path, directives_table)
	ast_node := p.parse_with_modname(base_module_name) or {
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

	// Extract constructs directly from parsed AST
	modules := extract_modules_from_ast(ast_node, base_module_name)!

	mut result := CompilationResult{
		main_module: base_module_name
		files:       map[string]string{}
	}

	for module_info in modules {
		mut gen := generator.new_generator(directives_table)
		erlang_code := gen.generate(module_info.ast_node) or {
			errs := gen.get_errors()
			if errs.len > 0 {
				return error('Generation errors for ${module_info.name}:\n${errs.join('\n')}')
			}
			return error('Generation error for ${module_info.name}: ${err}')
		}
		result.files[module_info.filename] = erlang_code
	}

	// Generate .app.src and .hrl best-effort from parsed AST
	result.app_src_content = generate_app_src(ast_node, base_module_name) or { '' }
	result.hrl_content = generate_hrl(ast_node, base_module_name) or { '' }

	return result
}

// Module information for multi-file generation
struct ModuleInfo {
	name     string
	filename string
	ast_node ast.Node
}

// Extract separate modules from main AST
fn extract_modules_from_ast(main_ast ast.Node, base_name string) ![]ModuleInfo {
	mut modules := []ModuleInfo{}
	mut main_children := []ast.Node{}
	mut records := []ast.Node{}
	mut application_nodes := []ast.Node{}

	// Separate constructs
	for child in main_ast.children {
		match child.kind {
			.application_config {
				application_nodes << child
			}
			.supervisor_def {
				// Create separate supervisor module
				sup_name := child.value
				sup_module_ast := ast.new_module(child.id, '${sup_name}_sup', [child],
					child.position)
				modules << ModuleInfo{
					name:     '${sup_name}_sup'
					filename: '${sup_name}_sup.erl'
					ast_node: sup_module_ast
				}
			}
			.worker_def {
				// Create separate worker module
				worker_name := child.value
				worker_module_ast := ast.new_module(child.id, worker_name, [child], child.position)
				modules << ModuleInfo{
					name:     worker_name
					filename: '${worker_name}.erl'
					ast_node: worker_module_ast
				}
			}
			.record_definition {
				records << child
			}
			else {
				main_children << child
			}
		}
	}

	// Create main module with remaining constructs + application configs as comments
	for app_node in application_nodes {
		main_children.insert(0, app_node)
	}
	for record in records {
		main_children.insert(0, record)
	}

	if main_children.len > 0 {
		main_module_ast := ast.new_module(main_ast.id, base_name, main_children, main_ast.position)
		modules << ModuleInfo{
			name:     base_name
			filename: '${base_name}.erl'
			ast_node: main_module_ast
		}
	}

	return modules
}

// Generate .app.src content from application config
fn generate_app_src(ast_node ast.Node, app_name string) !string {
	mut app_config := map[string]string{}
	mut deps_list := []string{}

	// Extract application config and deps declarations
	for child in ast_node.children {
		if child.kind == .application_config {
			// Parse key-value pairs from children
			for i := 0; i < child.children.len; i += 2 {
				key_node := child.children[i]
				value_node := child.children[i + 1]

				key := key_node.value
				// Special-case: deps lives inside application block
				if key == 'deps' {
					if value_node.kind == .list_literal {
						for dep_node in value_node.children {
							if dep_node.kind == .atom {
								deps_list << dep_node.value
							}
						}
					}
					// Do not include 'deps' in app_config output
					continue
				}

				value := format_app_src_value(value_node)!
				app_config[key] = value
			}
		}
	}

	if app_config.len == 0 && deps_list.len == 0 {
		return ''
	}

	// Set defaults
	if 'applications' !in app_config {
		mut applications := '[kernel, stdlib]'
		// Add declared deps to applications if any
		if deps_list.len > 0 {
			applications = '[kernel, stdlib, ' + deps_list.join(', ') + ']'
		}
		app_config['applications'] = applications
	} else if deps_list.len > 0 {
		// Merge existing applications with declared deps
		existing_apps := app_config['applications']
		if existing_apps.starts_with('[') && existing_apps.ends_with(']') {
			// Remove brackets and split
			apps_content := existing_apps[1..existing_apps.len - 1].trim_space()
			if apps_content.len > 0 {
				mut existing_list := apps_content.split(',').map(it.trim_space())
				// Add deps that aren't already in applications
				for dep in deps_list {
					if dep !in existing_list {
						existing_list << dep
					}
				}
				app_config['applications'] = '[' + existing_list.join(', ') + ']'
			} else {
				app_config['applications'] = '[kernel, stdlib, ' + deps_list.join(', ') + ']'
			}
		}
	}

	// Ensure mod entry exists so application starts in shell
	if 'mod' !in app_config {
		app_config['mod'] = '{${app_name}_app, []}'
	}

	mut content := '{application, ${app_name},\n [\n'

	// Required order: description, vsn, registered, applications, mod, env
	ordered_keys := ['description', 'vsn', 'registered', 'applications', 'mod', 'env']

	mut entries := []string{}
	for key in ordered_keys {
		if key in app_config {
			entries << '  {${key}, ${app_config[key]}}'
		}
	}

	// Add any remaining keys
	for key, value in app_config {
		if key !in ordered_keys {
			entries << '  {${key}, ${value}}'
		}
	}

	content += entries.join(',\n')
	content += '\n ]}.\n'

	return content
}

// Format AST node value for .app.src
fn format_app_src_value(node ast.Node) !string {
	match node.kind {
		.string {
			return '"${node.value}"'
		}
		.atom {
			return node.value
		}
		.integer, .float {
			return node.value
		}
		.list_literal {
			mut items := []string{}
			for child in node.children {
				items << format_app_src_value(child)!
			}
			return '[${items.join(', ')}]'
		}
		.map_literal {
			mut pairs := []string{}
			for i := 0; i < node.children.len; i += 2 {
				key := format_app_src_value(node.children[i])!
				value := format_app_src_value(node.children[i + 1])!
				pairs << '{${key}, ${value}}'
			}
			return '[${pairs.join(', ')}]'
		}
		else {
			return node.value
		}
	}
}

// Generate .hrl content from record definitions
fn generate_hrl(ast_node ast.Node, app_name string) !string {
	mut records := []ast.Node{}

	// Collect all record definitions
	for child in ast_node.children {
		if child.kind == .record_definition {
			records << child
		}
	}

	if records.len == 0 {
		return ''
	}

	mut content := '%% ${app_name}.hrl - Record definitions\n\n'

	for record in records {
		content += generate_record_hrl(record)! + '\n'
	}

	return content
}

// Generate single record definition for .hrl
fn generate_record_hrl(record_node ast.Node) !string {
	record_name := record_node.value
	mut fields := []string{}

	// Parse record fields from children
	for child in record_node.children {
		if child.kind == .identifier {
			field_name := child.value
			// Check for type annotation
			if child.children.len > 0 && child.children[0].kind == .identifier {
				field_type := child.children[0].value
				fields << '${field_name} :: ${field_type}()'
			} else {
				fields << field_name
			}
		}
	}

	return '-record(${record_name}, {${fields.join(', ')}}).'
}

// Generate a minimal application behaviour module, optionally starting a root supervisor
fn generate_application_behaviour_module(app_module_name string, root_supervisor_module string) string {
	mut content := ''
	content += '-module(' + app_module_name + ').\n'
	content += '-behaviour(application).\n\n'
	content += '-export([start/2, stop/1]).\n\n'
	content += 'start(_Type, _Args) ->\n'
	if root_supervisor_module.len > 0 {
		content += '    case ' + root_supervisor_module + ':start_link() of\n'
		content += '        {ok, Pid} -> {ok, Pid};\n'
		content += '        Error -> Error\n'
		content += '    end.\n\n'
	} else {
		content += '    {ok, self()}.\n\n'
	}
	content += 'stop(_State) ->\n'
	content += '    ok.\n'
	return content
}

pub fn compile_string(code string, file_path string) !string {
	module_name := os.file_name(file_path).all_before_last('.')
	return compile_string_with_modname(code, file_path, module_name)
}

pub fn compile_string_with_modname(code string, file_path string, module_name string) !string {
	mut directives_table := parser.new_directives_table()
	mut p := parser.new_parser(code, file_path, directives_table)
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

	mut analyzer := analysis.new_analyzer()
	analyzed_ast := analyzer.analyze(ast_node) or {
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

	mut gen := generator.new_generator(directives_table)
	result := gen.generate_with_types(analyzed_ast, analyzer.get_type_table()) or {
		errs := gen.get_errors()
		if errs.len > 0 {
			return error('Generate errors:\n' + errs.join('\n'))
		}
		return error('Generate error: ${err}')
	}

	return result
}

fn print_ast_nodes(node ast.Node, indent int) {
	pad := '  '.repeat(indent)
	println('${pad}- Node(id=${node.id}, kind=${node.kind}, value="${node.value}")')
	for child in node.children {
		print_ast_nodes(child, indent + 1)
	}
}
