module erlang

import ast
import analysis
import codegen { CodegenResult }
import frontend.parser.internal

// ErlangGenerator generates Erlang code from LX AST
pub struct ErlangGenerator {
mut:
	defined_types       map[string]ast.TypeAliasStmt // Map of type name to type definition
	var_scopes          []map[string]string          // Stack of variable scopes: original name -> hashed name
	next_hash           int                       // Counter for unique hashes
	type_context        ?&analysis.TypeContext    // Type context for record type information
	type_table          ?&analysis.TypeTable      // Type table from HM system
	current_function_id string                    // Current function ID for type lookups
	global_registry     ?&internal.GlobalRegistry // Global registry for cross-module types
}

// new_erlang_generator creates a new Erlang code generator
pub fn new_erlang_generator() ErlangGenerator {
	return ErlangGenerator{
		defined_types:       map[string]ast.TypeAliasStmt{}
		var_scopes:          [map[string]string{}]
		next_hash:           0
		type_context:        none
		type_table:          none
		current_function_id: ''
		global_registry:     none
	}
}

// set_global_registry sets the global registry for cross-module types
pub fn (mut gen ErlangGenerator) set_global_registry(registry &internal.GlobalRegistry) {
	gen.global_registry = unsafe { registry }
}

// set_type_table sets the type table for the generator
pub fn (mut gen ErlangGenerator) set_type_table(type_table &analysis.TypeTable) {
	gen.type_table = unsafe { type_table }
}

// generate_module generates a complete Erlang module (implements CodeGenerator interface)
pub fn (mut gen ErlangGenerator) generate_module(module_stmt ast.ModuleStmt, type_ctx &analysis.TypeContext) CodegenResult {
	gen.type_context = unsafe { type_ctx }
	// Collect all type definitions first
	gen.collect_type_definitions(module_stmt.statements)

	// Generate module header
	mut code := gen.get_module_header(module_stmt.name)

	// Generate behaviour directive if module has behaviour
	if behaviour := module_stmt.behaviour {
		match behaviour {
			ast.WorkerBehaviour {
				code += '-behaviour(gen_server).\n'
			}
			ast.SupervisorBehaviour {
				code += '-behaviour(supervisor).\n'
			}
		}
		code += '\n'
	}

	// Generate includes for cross-module types
	includes := gen.generate_includes(module_stmt.name)
	if includes.len > 0 {
		code += includes.join('\n') + '\n\n'
	}

	// Generate exports (including behaviour-specific exports)
	exports := gen.generate_exports_with_behaviour(module_stmt.statements, module_stmt.behaviour)
	if exports.len > 0 {
		code += '-export([${exports.join(', ')}]).\n'
	}
	code += '\n'

	// Generate statements (excluding shared records/types that go to .hrl)
	for stmt in module_stmt.statements {
		stmt_code := gen.generate_statement_filtered(stmt)
		code += stmt_code + '\n'
	}

	// Add behaviour-specific boilerplate for missing callbacks
	if behaviour := module_stmt.behaviour {
		boilerplate := gen.generate_behaviour_boilerplate(module_stmt.statements, behaviour)
		if boilerplate.len > 0 {
			code += '\n%% Default implementations (if not provided by user)\n'
			code += boilerplate
		}
	}

	// Add module footer
	code += gen.get_module_footer()

	// Generate .hrl content for all records/types in this module
	hrl_content := gen.generate_hrl_content(module_stmt.name, module_stmt.statements)

	return CodegenResult{
		success:     true
		errors:      []
		code:        code
		hrl_content: hrl_content
	}
}

// generate_functions_only generates only function code without module header/exports
pub fn (mut gen ErlangGenerator) generate_functions_only(statements []ast.Stmt, type_ctx &analysis.TypeContext) string {
	gen.type_context = unsafe { type_ctx }
	// Collect all type definitions first
	gen.collect_type_definitions(statements)

	mut code := ''

	// Generate only function statements (they already include specs)
	for stmt in statements {
		match stmt {
			ast.FunctionStmt {
				stmt_code := gen.generate_function(stmt)
				code += stmt_code
			}
			else {
				// Skip non-function statements for worker generation
			}
		}
	}

	return code
}

// collect_type_definitions collects all type alias definitions from module statements
fn (mut gen ErlangGenerator) collect_type_definitions(statements []ast.Stmt) {
	for stmt in statements {
		match stmt {
			ast.TypeAliasStmt {
				gen.defined_types[stmt.name] = stmt
			}
			else {
				// Skip non-type statements
			}
		}
	}
}

// generate_exports generates export list from module statements
fn (gen ErlangGenerator) generate_exports(statements []ast.Stmt) []string {
	mut exports := []string{}
	mut seen_exports := map[string]bool{}

	for stmt in statements {
		match stmt {
			ast.FunctionStmt {
				// Only export public functions (not private ones)
				if !stmt.is_private {
					// Generate exports for all clauses with different arities
					for clause in stmt.clauses {
						param_count := clause.parameters.len
						export_name := '${stmt.name}/${param_count}'
						if !seen_exports[export_name] {
							exports << export_name
							seen_exports[export_name] = true
						}
					}
				}
			}
			else {
				// Skip non-function statements
			}
		}
	}

	return exports
}

// generate_exports_with_behaviour generates export list including behaviour-specific exports
fn (gen ErlangGenerator) generate_exports_with_behaviour(statements []ast.Stmt, behaviour ?ast.Behaviour) []string {
	mut exports := gen.generate_exports(statements)

	// Add behaviour-specific exports
	if behaviour_val := behaviour {
		match behaviour_val {
			ast.WorkerBehaviour {
				// Add gen_server callback exports if not already present
				required_callbacks := ['init/1', 'handle_call/3', 'handle_cast/2', 'handle_info/2', 'terminate/2', 'code_change/3']
				for callback in required_callbacks {
					if callback !in exports {
						exports << callback
					}
				}
			}
			ast.SupervisorBehaviour {
				// Add supervisor callback exports if not already present
				required_callbacks := ['init/1']
				for callback in required_callbacks {
					if callback !in exports {
						exports << callback
					}
				}
			}
		}
	}

	return exports
}

// generate_includes generates include statements for cross-module types
fn (gen ErlangGenerator) generate_includes(module_name string) []string {
	mut includes := []string{}

	if registry := gen.global_registry {
		// Check which external modules this module depends on
		mut external_modules := map[string]bool{}

		// Check record usages
		for record_key, consumers in registry.record_usages {
			if module_name in consumers {
				parts := record_key.split('.')
				if parts.len == 2 {
					external_modules[parts[0]] = true
				}
			}
		}

		// Check type usages
		for type_key, consumers in registry.type_usages {
			if module_name in consumers {
				parts := type_key.split('.')
				if parts.len == 2 {
					external_modules[parts[0]] = true
				}
			}
		}

		// Check if this module has its own records (needs self-include)
		for record_key, _ in registry.records {
			parts := record_key.split('.')
			if parts.len == 2 && parts[0] == module_name {
				// This module defines records, so it needs to include its own .hrl
				external_modules[module_name] = true
				break
			}
		}

		// For child modules (workers/supervisors), also check if they need parent module records
		// This is a simple heuristic: if the module name is not the main module, include main module .hrl
		mut main_module_candidates := []string{}
		for record_key, _ in registry.records {
			parts := record_key.split('.')
			if parts.len == 2 {
				main_module_candidates << parts[0]
			}
		}

		// If this module is not in the main candidates but there are records defined,
		// include the first main module (typically the parent module)
		if module_name !in main_module_candidates && main_module_candidates.len > 0 {
			external_modules[main_module_candidates[0]] = true
		}

		// Generate include statements
		for ext_module, _ in external_modules {
			includes << '-include("${ext_module}.hrl").'
		}
	}

	return includes
}

// generate_statement_filtered generates code for statements, filtering out shared records/types
fn (mut gen ErlangGenerator) generate_statement_filtered(stmt ast.Stmt) string {
	match stmt {
		ast.RecordDefStmt {
			// Always skip records - they go to .hrl
			return ''
		}
		ast.TypeAliasStmt {
			// Always skip type aliases - they go to .hrl
			return ''
		}
		else {
			// Generate other statements normally
			return gen.generate_statement(stmt)
		}
	}
}

// generate_hrl_content generates .hrl content for all records and types in the module
fn (mut gen ErlangGenerator) generate_hrl_content(module_name string, statements []ast.Stmt) string {
	mut all_records := []ast.RecordDefStmt{}
	mut all_types := []ast.TypeAliasStmt{}

	// Collect all records and types
	for stmt in statements {
		match stmt {
			ast.RecordDefStmt {
				all_records << stmt
			}
			ast.TypeAliasStmt {
				all_types << stmt
			}
			else {}
		}
	}

	// Generate .hrl content if there are any records or types
	if all_records.len > 0 || all_types.len > 0 {
		mut hrl_content := ''

		// Add header comment
		hrl_content += '%% Generated header file for ${module_name} module\n'
		hrl_content += '%% Contains records and type definitions\n\n'

		// Generate record definitions
		for record in all_records {
			hrl_content += gen.generate_record_definition(record) + '\n'
		}

		// Generate type definitions
		for type_alias in all_types {
			hrl_content += gen.generate_type_alias(type_alias) + '\n'
		}

		return hrl_content
	}

	return ''
}

// Interface implementations
pub fn (gen ErlangGenerator) get_file_extension() string {
	return '.erl'
}

pub fn (gen ErlangGenerator) get_module_header(module_name string) string {
	return '-module(${module_name}).\n'
}

pub fn (gen ErlangGenerator) get_module_footer() string {
	return ''
}

// Scope management for variable hashing
// Call this when entering a new block scope
fn (mut gen ErlangGenerator) enter_scope() {
	gen.var_scopes << map[string]string{}
}

// Call this when exiting a block scope
fn (mut gen ErlangGenerator) exit_scope() {
	if gen.var_scopes.len > 1 {
		gen.var_scopes.delete_last()
	}
}

// Generates and binds a unique hashed variable name for a new variable binding
// If is_param is true, do not add hash (for function parameters only)
fn (mut gen ErlangGenerator) bind_variable(name string, is_param bool) string {
	if is_param {
		// For function parameters, do not add hash
		simple_name := gen.capitalize_variable(name)
		gen.var_scopes.last()[name] = simple_name
		return simple_name
	} else {
		// For all other variables, always add hash
		hash := gen.generate_alphanumeric_hash()
		gen.next_hash++
		hashed := gen.capitalize_variable(name) + '_' + hash
		gen.var_scopes.last()[name] = hashed
		return hashed
	}
}

// generate_alphanumeric_hash generates a 4-character lowercase alphanumeric hash
fn (mut gen ErlangGenerator) generate_alphanumeric_hash() string {
	// Use a simple algorithm to generate consistent 4-char alphanumeric strings
	chars := 'abcdefghijklmnopqrstuvwxyz0123456789'
	mut hash := ''
	mut num := gen.next_hash

	for _ in 0 .. 4 {
		index := num % chars.len
		hash += chars[index].ascii_str()
		num = num / chars.len
	}

	return hash
}

// Looks up the hashed variable name for a given original name, searching from innermost to outermost scope
fn (gen ErlangGenerator) lookup_variable(name string) string {
	for i := gen.var_scopes.len - 1; i >= 0; i-- {
		if name in gen.var_scopes[i] {
			return gen.var_scopes[i][name]
		}
	}
	return gen.capitalize_variable(name) // fallback (should not happen)
}

// generate_behaviour_boilerplate generates default implementations for missing behaviour callbacks
fn (gen ErlangGenerator) generate_behaviour_boilerplate(statements []ast.Stmt, behaviour ast.Behaviour) string {
	// Check which functions the user has implemented
	mut user_functions := map[string]bool{}
	for stmt in statements {
		if stmt is ast.FunctionStmt {
			func_stmt := stmt as ast.FunctionStmt
			// Create function signature for lookup
			arity := func_stmt.clauses[0].parameters.len
			signature := '${func_stmt.name}/${arity}'
			user_functions[signature] = true
		}
	}

	mut boilerplate := ''

	match behaviour {
		ast.WorkerBehaviour {
			// Gen_server callbacks
			if !user_functions['handle_cast/2'] {
				boilerplate += 'handle_cast(_Msg, State) ->\n    {noreply, State}.\n\n'
			}
			if !user_functions['handle_info/2'] {
				boilerplate += 'handle_info(_Info, State) ->\n    {noreply, State}.\n\n'
			}
			if !user_functions['terminate/2'] {
				boilerplate += 'terminate(_Reason, _State) ->\n    ok.\n\n'
			}
			if !user_functions['code_change/3'] {
				boilerplate += 'code_change(_OldVsn, State, _Extra) ->\n    {ok, State}.\n\n'
			}
		}
		ast.SupervisorBehaviour {
			// Generate default init/1 for supervisor if not provided by user
			if !user_functions['init/1'] {
				children_spec := gen.generate_children_spec_code(behaviour.children)
				strategy_atom := match behaviour.strategy {
					.one_for_one { 'one_for_one' }
					.one_for_all { 'one_for_all' }
					.rest_for_one { 'rest_for_one' }
				}
				boilerplate += 'init([]) ->\n    {ok, {{${strategy_atom}, 5, 10}, [\n        ${children_spec}\n    ]}}.\n\n'
			}
		}
	}

	return boilerplate
}

// generate_children_spec_code generates Erlang children specification code
fn (gen ErlangGenerator) generate_children_spec_code(children_spec ast.ChildrenSpec) string {
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
