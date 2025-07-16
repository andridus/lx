module compiler

import frontend.lexer
import frontend.parser
import frontend.parser.internal
import os
import ast
import errors
import analysis
import backend.codegen
import backend.erlang

// CompilerResult represents the result of compilation
pub struct CompilerResult {
pub:
	success     bool
	erlang_code string
	module_name string
	file_path   string
	errors      []string
	warnings    []string
}

// Compiler represents the main compiler for LX language
pub struct Compiler {
pub mut:
	file_path    string
	module_name  string
	debug_tokens bool
	debug_types  bool // Flag to enable type debugging
}

// new_compiler creates a new compiler instance
pub fn new_compiler() Compiler {
	return Compiler{
		module_name:  ''
		file_path:    ''
		debug_tokens: false
		debug_types:  false
	}
}

// enable_debug_tokens enables token debugging
pub fn (mut comp Compiler) enable_debug_tokens() {
	comp.debug_tokens = true
}

// enable_debug_types enables type debugging
pub fn (mut comp Compiler) enable_debug_types() {
	comp.debug_types = true
}

// compile_file compiles a single file
pub fn (mut comp Compiler) compile_file(file_path string) {
	module_name := os.file_name(file_path).replace('.lx', '')
	comp.file_path = file_path
	// Read the source file
	source := os.read_file(file_path) or {
		println('Failed to read file: ${file_path}: ${err}')
		exit(1)
	}
	result := comp.compile(source, file_path, '')

	// Create output directory if it doesn't exist
	os.mkdir_all(os.dir(file_path)) or {
		println('Failed to create output directory: ${err}')
		exit(1)
	}

	// Generate output filename in the same directory
	output_file := '${os.dir(file_path)}/${module_name}.erl'

	// Write the Erlang code to file
	os.write_file(output_file, result.code) or {
		println('Failed to write output file: ${err}')
		exit(1)
	}
}

pub fn (mut comp Compiler) compile(source string, file_path string, output_dir string) codegen.CodegenResult {
	mut error_formatter := errors.new_error_formatter()
	// Create global registry for cross-module types
	global_registry := internal.new_global_registry()

	// Create lexer and tokenize
	mut lexer_instance := lexer.new_lexer(source, file_path)
	mut tokens := []lexer.Token{}
	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			println('Lexical error: ${token.message}')
			exit(1)
		}
		tokens << token
	}

	if comp.debug_tokens {
		println('=== TOKENS ===')
		for i, token in tokens {
			println('${i}: ${token.str()} @ ${token.get_position().str()}')
		}
		println('=== END TOKENS ===')
	}
	if lexer_instance.has_errors() {
		// Format lexer errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in lexer_instance.get_errors() {
			formatted_errors << error_formatter.format_error(error, source_lines)
		}
		println('Lexical errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	// Create parser and parse the tokens into AST
	module_name := os.file_name(file_path).replace('.lx', '')
	mut parser1_instance := parser.new_parser(tokens, module_name, global_registry)
	program_stmt := parser1_instance.parse_program() or { ast.ModuleStmt{} }

	if parser1_instance.has_errors() {
		// Format parser errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in parser1_instance.get_errors() {
			formatted_errors << error_formatter.format_error(error, source_lines)
		}
		println('Parser errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	// Check if this is an application or module
	match program_stmt {
		ast.ApplicationStmt {
			// Handle application compilation
			return comp.compile_application(program_stmt, file_path)
		}
		ast.ModuleStmt {
			// Handle module compilation (existing behavior)
			return comp.compile_module_with_workers_supervisors(program_stmt, file_path, output_dir, global_registry)
		}
		else {
			println('Error: Unsupported program type')
			exit(1)
		}
	}
}

// compile_module compiles a single module
fn (mut comp Compiler) compile_module(module_stmt ast.ModuleStmt, file_path string, output_dir string, global_registry &internal.GlobalRegistry) codegen.CodegenResult {
	// Use the HM type system (now the only option)
	mut analyzer := if comp.debug_types {
		println('Using Hindley-Milner type inference with debug')
		analysis.new_analyzer_with_debug()
	} else {
		println('Using Hindley-Milner type inference')
		analysis.new_analyzer()
	}

	analysis_result := analyzer.analyze_module(module_stmt)

	// Print debug information if requested
	if comp.debug_types {
		println('\n=== TYPE ANALYSIS DEBUG ===')
		analyzer.print_debug()
		println('============================\n')
	}

	if analysis_result.errors.len > 0 {
		source_lines := errors.load_source_lines(file_path)
		mut error_formatter := errors.new_error_formatter()
		mut formatted_errors := []string{}
		for err in analysis_result.errors {
			formatted_errors << error_formatter.format_error(err, source_lines)
		}
		println('Analysis errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	mut final_module_stmt := module_stmt
	module_name := os.file_name(file_path).replace('.lx', '')
	if final_module_stmt.name == 'main' {
		final_module_stmt.name = module_name
	}

	mut erlang_gen := erlang.new_erlang_generator()
	// Set the type table in the backend (HM is now the only system)
	type_table := analyzer.get_type_table()
	erlang_gen.set_type_table(type_table)
	// Set the global registry for cross-module types
	erlang_gen.set_global_registry(global_registry)

	codegen_result := erlang_gen.generate_module(final_module_stmt, analysis_result.type_context)

	if !codegen_result.success {
		eprintln('Code generation failed')
		for error in codegen_result.errors {
			eprintln('Error: ${error}')
		}
		exit(1)
	}

	// Generate .hrl file if there's content for it
	if codegen_result.hrl_content.len > 0 {
		hrl_dir := if output_dir.len > 0 { output_dir } else { os.dir(file_path) }
		hrl_file := '${hrl_dir}/${final_module_stmt.name}.hrl'
		os.write_file(hrl_file, codegen_result.hrl_content) or {
			eprintln('Failed to write ${hrl_file}: ${err}')
			exit(1)
		}
		println('Generated ${hrl_file}')
	}

	println('Compiled ${file_path} successfully')
	return codegen_result
}

// compile_module_with_workers_supervisors compiles a module and generates separate files for workers and supervisors
fn (mut comp Compiler) compile_module_with_workers_supervisors(module_stmt ast.ModuleStmt, file_path string, output_dir string, global_registry &internal.GlobalRegistry) codegen.CodegenResult {
	// First, separate workers and supervisors from regular statements
	mut regular_statements := []ast.Stmt{}
	mut workers := []ast.WorkerStmt{}
	mut supervisors := []ast.SupervisorStmt{}

	for stmt in module_stmt.statements {
		match stmt {
			ast.WorkerStmt {
				workers << stmt
			}
			ast.SupervisorStmt {
				supervisors << stmt
			}
			else {
				regular_statements << stmt
			}
		}
	}

	// Create a new module with only regular statements
	regular_module := ast.ModuleStmt{
		...module_stmt
		statements: regular_statements
	}

	// Compile the regular module
	main_result := comp.compile_module(regular_module, file_path, output_dir, global_registry)

	// Generate separate files for workers
	for worker in workers {
		comp.generate_worker_file(worker, file_path, output_dir, global_registry)
	}

	// Generate separate files for supervisors
	for supervisor in supervisors {
		comp.generate_supervisor_file(supervisor, file_path, output_dir, global_registry)
	}

	return main_result
}

// generate_worker_file generates a separate .erl file for a worker
fn (mut comp Compiler) generate_worker_file(worker_stmt ast.WorkerStmt, original_file_path string, output_dir string, global_registry &internal.GlobalRegistry) {
	// Create a module for the worker
	worker_module := ast.ModuleStmt{
		name: worker_stmt.name
		exports: ['start_link/1', 'init/1'] // Basic OTP gen_server exports
		imports: []
		statements: worker_stmt.statements
		position: worker_stmt.position
	}

	// Compile the worker module
	result := comp.compile_module(worker_module, original_file_path, output_dir, global_registry)

	// Write the worker file
	worker_dir := if output_dir.len > 0 { output_dir } else { os.dir(original_file_path) }
	worker_file := '${worker_dir}/${worker_stmt.name}.erl'

	// Generate OTP gen_server boilerplate
	worker_code := comp.generate_worker_boilerplate(worker_stmt, result.code)

	os.write_file(worker_file, worker_code) or {
		println('Failed to write worker file ${worker_file}: ${err}')
		exit(1)
	}

	println('Generated worker: ${worker_file}')
}

// generate_supervisor_file generates a separate .erl file for a supervisor
fn (mut comp Compiler) generate_supervisor_file(supervisor_stmt ast.SupervisorStmt, original_file_path string, output_dir string, global_registry &internal.GlobalRegistry) {
	supervisor_name := if supervisor_stmt.name == '' { 'main_supervisor' } else { supervisor_stmt.name }

	// Create a module for the supervisor
	supervisor_module := ast.ModuleStmt{
		name: supervisor_name
		exports: ['start_link/0', 'init/1'] // Basic OTP supervisor exports
		imports: []
		statements: supervisor_stmt.statements
		position: supervisor_stmt.position
	}

	// Compile the supervisor module
	result := comp.compile_module(supervisor_module, original_file_path, output_dir, global_registry)

	// Write the supervisor file
	supervisor_dir := if output_dir.len > 0 { output_dir } else { os.dir(original_file_path) }
	supervisor_file := '${supervisor_dir}/${supervisor_name}.erl'

	// Generate OTP supervisor boilerplate
	supervisor_code := comp.generate_supervisor_boilerplate(supervisor_stmt, result.code)

	os.write_file(supervisor_file, supervisor_code) or {
		println('Failed to write supervisor file ${supervisor_file}: ${err}')
		exit(1)
	}

	println('Generated supervisor: ${supervisor_file}')
}

// generate_worker_boilerplate generates OTP gen_server boilerplate for workers
fn (mut comp Compiler) generate_worker_boilerplate(worker_stmt ast.WorkerStmt, user_code string) string {
	return '-module(${worker_stmt.name}).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API functions
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% User-defined functions and records
${user_code}

%% Default implementations (if not provided by user)
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
'
}

// generate_supervisor_boilerplate generates OTP supervisor boilerplate
fn (mut comp Compiler) generate_supervisor_boilerplate(supervisor_stmt ast.SupervisorStmt, user_code string) string {
	supervisor_name := if supervisor_stmt.name == '' { 'main_supervisor' } else { supervisor_stmt.name }

	// Generate children specification
	children_spec := comp.generate_children_spec(supervisor_stmt.children)

	// Convert strategy to Erlang atom
	strategy_atom := match supervisor_stmt.strategy {
		.one_for_one { 'one_for_one' }
		.one_for_all { 'one_for_all' }
		.rest_for_one { 'rest_for_one' }
	}

	return '-module(${supervisor_name}).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
    {ok, {{${strategy_atom}, 5, 10}, [
        ${children_spec}
    ]}}.

%% User-defined functions and records
${user_code}
'
}

// generate_children_spec generates Erlang children specification
fn (mut comp Compiler) generate_children_spec(children_spec ast.ChildrenSpec) string {
	mut specs := []string{}

	match children_spec {
		ast.ListChildren {
			for child in children_spec.children {
				specs << '{${child}, {${child}, start_link, []}, permanent, 5000, worker, [${child}]}'
			}
		}
		ast.MapChildren {
			// Add workers
			for worker in children_spec.workers {
				specs << '{${worker}, {${worker}, start_link, []}, permanent, 5000, worker, [${worker}]}'
			}
			// Add supervisors
			for supervisor in children_spec.supervisors {
				specs << '{${supervisor}, {${supervisor}, start_link, []}, permanent, infinity, supervisor, [${supervisor}]}'
			}
		}
		ast.TupleChildren {
			for child in children_spec.children {
				specs << '{${child.name}, {${child.name}, start_link, []}, ${child.restart}, ${child.shutdown}, ${child.type_}, [${child.name}]}'
			}
		}
	}

	return specs.join(',\n        ')
}

// compile_application compiles an application with multiple modules
fn (mut comp Compiler) compile_application(app_stmt ast.ApplicationStmt, file_path string) codegen.CodegenResult {
	// Use the ApplicationGenerator from backend/erlang
	mut app_gen := erlang.new_application_generator()

	// Extract values from the fields map
	description := app_gen.get_string_field(app_stmt.fields, 'description') or { 'Lx Application' }

	println('Compiling application: ${description}')

	// Create output directory
	app_name := os.file_name(file_path).replace('.lx', '')
	output_dir := '${app_name}'

	// Create directory structure
	os.mkdir_all('${output_dir}/src') or {
		println('Error creating src directory: ${err}')
		exit(1)
	}

	// Generate rebar.config
	rebar_config := app_gen.generate_rebar_config(app_stmt)
	os.write_file('${output_dir}/rebar.config', rebar_config) or {
		println('Error writing rebar.config: ${err}')
		exit(1)
	}

	// Generate .app.src file
	app_src := app_gen.generate_app_src(app_stmt, app_name)
	os.write_file('${output_dir}/src/${app_name}.app.src', app_src) or {
		println('Error writing .app.src: ${err}')
		exit(1)
	}

	// Generate <app_name>.erl with basic structure
	main_erl := app_gen.generate_main_erl(app_stmt, app_name)
	os.write_file('${output_dir}/src/${app_name}.erl', main_erl) or {
		println('Error writing ${app_name}.erl: ${err}')
		exit(1)
	}

	println('Application structure generated in: ${output_dir}')
	println('Run "cd ${output_dir} && rebar3 compile" to build the application')

	// Return success result
	return codegen.CodegenResult{
		success:     true
		code:        ''
		file_path:   '${output_dir}/src/${app_name}.erl'
		errors:      []
		warnings:    []
		module_name: app_name
	}
}

// WorkerSupervisorResult holds the results of worker/supervisor compilation for testing
pub struct WorkerSupervisorResult {
pub:
	main_code       string
	main_hrl        string
	worker_files    map[string]string  // module_name -> erlang_code
	supervisor_files map[string]string // module_name -> erlang_code
}

// compile_for_testing compiles source code and returns worker/supervisor strings for testing
pub fn (mut comp Compiler) compile_for_testing(source string, file_path string) WorkerSupervisorResult {
	mut error_formatter := errors.new_error_formatter()
	// Create global registry for cross-module types
	global_registry := internal.new_global_registry()

	// Create lexer and tokenize
	mut lexer_instance := lexer.new_lexer(source, file_path)
	mut tokens := []lexer.Token{}
	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			println('Lexical error: ${token.message}')
			exit(1)
		}
		tokens << token
	}

	if lexer_instance.has_errors() {
		// Format lexer errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in lexer_instance.get_errors() {
			formatted_errors << error_formatter.format_error(error, source_lines)
		}
		println('Lexical errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	// Create parser and parse the tokens into AST
	module_name := os.file_name(file_path).replace('.lx', '')
	mut parser1_instance := parser.new_parser(tokens, module_name, global_registry)
	program_stmt := parser1_instance.parse_program() or { ast.ModuleStmt{} }

	if parser1_instance.has_errors() {
		// Format parser errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in parser1_instance.get_errors() {
			formatted_errors << error_formatter.format_error(error, source_lines)
		}
		println('Parser errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	// Check if this is an application or module
	match program_stmt {
		ast.ApplicationStmt {
			// Handle application compilation - for now just return empty
			return WorkerSupervisorResult{
				main_code: '-module(test).\n\n'
				main_hrl: ''
				worker_files: {}
				supervisor_files: {}
			}
		}
		ast.ModuleStmt {
			// Handle module compilation
			return comp.compile_module_for_testing(program_stmt, file_path, global_registry)
		}
		else {
			println('Error: Unsupported program type')
			exit(1)
		}
	}
}

// compile_module_for_testing compiles a module and returns worker/supervisor strings for testing
fn (mut comp Compiler) compile_module_for_testing(module_stmt ast.ModuleStmt, file_path string, global_registry &internal.GlobalRegistry) WorkerSupervisorResult {
	// First, separate workers and supervisors from regular statements
	mut regular_statements := []ast.Stmt{}
	mut workers := []ast.WorkerStmt{}
	mut supervisors := []ast.SupervisorStmt{}

	for stmt in module_stmt.statements {
		match stmt {
			ast.WorkerStmt {
				workers << stmt
			}
			ast.SupervisorStmt {
				supervisors << stmt
			}
			else {
				regular_statements << stmt
			}
		}
	}

	// Create a new module with only regular statements
	regular_module := ast.ModuleStmt{
		...module_stmt
		statements: regular_statements
	}

	// Compile the regular module
	main_result := comp.compile_module(regular_module, file_path, '', global_registry)

	// Generate worker strings
	mut worker_files := map[string]string{}
	for worker in workers {
		worker_code := comp.generate_worker_string(worker, file_path, global_registry)
		worker_files[worker.name] = worker_code
	}

	// Generate supervisor strings
	mut supervisor_files := map[string]string{}
	for supervisor in supervisors {
		supervisor_code := comp.generate_supervisor_string(supervisor, file_path, global_registry)
		supervisor_files[supervisor.name] = supervisor_code
	}

	return WorkerSupervisorResult{
		main_code: main_result.code
		main_hrl: main_result.hrl_content
		worker_files: worker_files
		supervisor_files: supervisor_files
	}
}

// generate_worker_string generates worker code as string for testing
fn (mut comp Compiler) generate_worker_string(worker_stmt ast.WorkerStmt, original_file_path string, global_registry &internal.GlobalRegistry) string {
	// Create a temporary module with just the worker functions
	temp_module := ast.ModuleStmt{
		name: worker_stmt.name
		statements: worker_stmt.statements
	}

	// Compile the worker functions
	result := comp.compile_module(temp_module, original_file_path, '', global_registry)

	// Generate complete worker boilerplate
	return comp.generate_worker_boilerplate(worker_stmt, result.code)
}

// generate_supervisor_string generates supervisor code as string for testing
fn (mut comp Compiler) generate_supervisor_string(supervisor_stmt ast.SupervisorStmt, original_file_path string, global_registry &internal.GlobalRegistry) string {
	// Create a temporary module with just the supervisor functions (if any)
	temp_module := ast.ModuleStmt{
		name: supervisor_stmt.name
		statements: []
	}

	// Compile the supervisor functions
	result := comp.compile_module(temp_module, original_file_path, '', global_registry)

	// Generate complete supervisor boilerplate
	return comp.generate_supervisor_boilerplate(supervisor_stmt, result.code)
}
