module erlang

import ast

// ApplicationGenerator generates Erlang application files
pub struct ApplicationGenerator {
}

// new_application_generator creates a new application generator
pub fn new_application_generator() ApplicationGenerator {
	return ApplicationGenerator{}
}

// generate_app_src generates .app.src content
pub fn (gen ApplicationGenerator) generate_app_src(app_stmt ast.ApplicationStmt, app_name string) string {
	description := gen.get_string_field(app_stmt.fields, 'description') or { 'Lx Application' }
	version := gen.get_string_field(app_stmt.fields, 'vsn') or { '1.0.0' }

	// Extract applications list
	applications := gen.get_atom_list_field(app_stmt.fields, 'applications') or { ['kernel', 'stdlib'] }

	// Extract registered list
	registered := gen.get_atom_list_field(app_stmt.fields, 'registered') or { [] }

	// Extract env map
	env_map := gen.get_env_map_field(app_stmt.fields, 'env') or { map[string]string{} }

	mut app_src := '{application, ${app_name}, [\n'
	app_src += '    {description, "${description}"},\n'
	app_src += '    {vsn, "${version}"},\n'
	app_src += '    {modules, []},\n'

	// Applications
	app_src += '    {applications, ['
	for i, app in applications {
		app_src += app
		if i < applications.len - 1 {
			app_src += ', '
		}
	}
	app_src += ']},\n'

	// Registered processes
	if registered.len > 0 {
		app_src += '    {registered, ['
		for i, reg in registered {
			app_src += reg
			if i < registered.len - 1 {
				app_src += ', '
			}
		}
		app_src += ']},\n'
	}

	// Environment
	if env_map.len > 0 {
		app_src += '    {env, ['
		mut env_pairs := []string{}
		for key, value in env_map {
			env_pairs << '{${key}, "${value}"}'
		}
		app_src += env_pairs.join(', ')
		app_src += ']},\n'
	}

	app_src += '    {mod, {main, []}}\n'
	app_src += ']}.\n'

	return app_src
}

// generate_rebar_config generates rebar.config content
pub fn (gen ApplicationGenerator) generate_rebar_config(app_stmt ast.ApplicationStmt) string {
	mut config := '{deps, [\n'

	// Extract dependencies from fields map
	if deps_expr := app_stmt.fields['deps'] {
		deps := gen.extract_dependencies(deps_expr)
		for i, dep in deps {
			if version := dep.version {
				config += '    {${dep.name}, "${version}"}'
			} else {
				config += '    ${dep.name}'
			}
			if i < deps.len - 1 {
				config += ','
			}
			config += '\n'
		}
	}

	config += ']}.\n\n'
	config += '{erl_opts, [debug_info]}.\n\n'
	config += '{shell, [\n'
	config += '    {config, "config/sys.config"}\n'
	config += ']}.\n\n'
	config += '{src_dirs, ["src"]}.\n'
	config += '{include_dirs, ["include"]}.\n'

	return config
}

// generate_main_erl generates <app_name>.erl content
pub fn (gen ApplicationGenerator) generate_main_erl(app_stmt ast.ApplicationStmt, app_name string) string {
	description := gen.get_string_field(app_stmt.fields, 'description') or { 'Lx Application' }

	return '-module(${app_name}).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ${description}

start(_StartType, _StartArgs) ->
    {ok, self()}.

stop(_State) ->
    ok.
'
}

// Helper functions to extract values from fields map

// get_string_field extracts a string value from a field
pub fn (gen ApplicationGenerator) get_string_field(fields map[string]ast.Expr, field_name string) ?string {
	if expr := fields[field_name] {
		if expr is ast.LiteralExpr {
			if expr.value is ast.StringLiteral {
				return expr.value.value
			}
		}
	}
	return none
}

// get_atom_list_field extracts an atom list from a field
pub fn (gen ApplicationGenerator) get_atom_list_field(fields map[string]ast.Expr, field_name string) ?[]string {
	if expr := fields[field_name] {
		if expr is ast.ListLiteralExpr {
			mut atoms := []string{}
			for element in expr.elements {
				if element is ast.LiteralExpr {
					if element.value is ast.AtomLiteral {
						atoms << element.value.value
					}
				}
			}
			return atoms
		}
	}
	return none
}

// get_env_map_field extracts an environment map from a field
pub fn (gen ApplicationGenerator) get_env_map_field(fields map[string]ast.Expr, field_name string) ?map[string]string {
	if expr := fields[field_name] {
		if expr is ast.MapLiteralExpr {
			mut env_map := map[string]string{}
			for entry in expr.entries {
				if entry.key is ast.LiteralExpr && entry.value is ast.LiteralExpr {
					key_expr := entry.key as ast.LiteralExpr
					value_expr := entry.value as ast.LiteralExpr

					if key_expr.value is ast.AtomLiteral {
						key_str := key_expr.value.value
						match value_expr.value {
							ast.StringLiteral {
								env_map[key_str] = value_expr.value.value
							}
							ast.AtomLiteral {
								env_map[key_str] = value_expr.value.value
							}
							ast.BooleanLiteral {
								env_map[key_str] = if value_expr.value.value { 'true' } else { 'false' }
							}
							ast.IntegerLiteral {
								env_map[key_str] = value_expr.value.value.str()
							}
							else {}
						}
					}
				}
			}
			return env_map
		}
	}
	return none
}

// extract_dependencies extracts dependencies from a list expression
pub fn (gen ApplicationGenerator) extract_dependencies(expr ast.Expr) []ast.Dependency {
	mut deps := []ast.Dependency{}

	if expr is ast.ListLiteralExpr {
		for element in expr.elements {
			if element is ast.LiteralExpr {
				if element.value is ast.AtomLiteral {
					// Simple dependency like :cowboy
					deps << ast.Dependency{
						name: element.value.value
						version: none
						source: none
						position: element.position
					}
				}
			} else if element is ast.TupleExpr {
				// Complex dependency like {:mongodb, "~> 4.0.0"}
				if element.elements.len == 2 {
					if element.elements[0] is ast.LiteralExpr && element.elements[1] is ast.LiteralExpr {
						name_expr := element.elements[0] as ast.LiteralExpr
						version_expr := element.elements[1] as ast.LiteralExpr

						if name_expr.value is ast.AtomLiteral && version_expr.value is ast.StringLiteral {
							deps << ast.Dependency{
								name: name_expr.value.value
								version: version_expr.value.value
								source: none
								position: element.position
							}
						}
					}
				}
			}
		}
	}

	return deps
}