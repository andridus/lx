module erlang

import ast
import typechecker.types
import generate.context
import generate

// ErlangGenerator implements the CodeGenerator interface for Erlang
pub struct ErlangGenerator {
	generate.BaseCodeGenerator
}

// new_erlang_generator creates a new Erlang code generator
pub fn new_erlang_generator() ErlangGenerator {
	return ErlangGenerator{}
}

// generate_module generates a complete Erlang module
pub fn (gen ErlangGenerator) generate_module(mod ast.Module, type_ctx &types.Context) generate.CodegenResult {
	mut ctx := generate.context.new_context(mod.name, mod.path, type_ctx)

	// Generate module header
	ctx.writeln(gen.get_module_header(mod.name))

	// Generate exports if there are functions
	if mod.statements.len > 0 {
		exports := gen.generate_exports(mod.statements)
		if exports.len > 0 {
			ctx.writeln('-export([${exports.join(', ')}]).')
			ctx.writeln('')
		}
	}

	// Generate each statement
	for stmt in mod.statements {
		generated := gen.generate_statement(stmt, mut ctx)
		if generated.len > 0 {
			ctx.writeln(generated)
			ctx.writeln('')
		}
	}

	// Generate module footer
	footer := gen.get_module_footer()
	if footer.len > 0 {
		ctx.writeln(footer)
	}

	success := !ctx.has_errors()
	file_path := '${mod.name}${gen.get_file_extension()}'

	return generate.codegen.new_result(success, ctx.get_code(), mod.name, file_path)
}

// generate_exports generates export declarations for functions
pub fn (gen ErlangGenerator) generate_exports(statements []ast.Statement) []string {
	mut exports := []string{}

	for stmt in statements {
		match stmt {
			ast.FunctionDefinition {
				function_name := stmt.function.name
				arity := stmt.function.clauses[0].parameters.len
				exports << '${function_name}/${arity}'
			}
			else {}
		}
	}

	return exports
}

// Override specific methods for Erlang-specific generation

// generate_literal overrides base implementation for Erlang-specific literals
pub fn (gen ErlangGenerator) generate_literal(literal ast.Literal) string {
	match literal {
		ast.IntegerLiteral {
			return literal.value.str()
		}
		ast.FloatLiteral {
			return literal.value.str()
		}
		ast.StringLiteral {
			// Escape quotes and special characters for Erlang
			escaped := literal.value.replace('\\', '\\\\').replace('"', '\\"')
			return "\"${escaped}\""
		}
		ast.BooleanLiteral {
			return if literal.value { 'true' } else { 'false' }
		}
		ast.AtomLiteral {
			// Erlang atoms don't need quotes unless they contain special characters
			if literal.value.contains(' ') || literal.value.contains('-') {
				return "'${literal.value}'"
			}
			return literal.value
		}
		ast.NilLiteral {
			return 'nil'
		}
	}
}

// generate_identifier overrides base implementation for Erlang variable naming
pub fn (gen ErlangGenerator) generate_identifier(ident ast.Identifier) string {
	if gen.context == unsafe { nil } {
		return gen.capitalize_variable(ident.name)
	}

	if variable := gen.context.find_variable(ident.name) {
		return variable.generated_name
	}

	// Generate Erlang-style variable name
	return gen.capitalize_variable(ident.name)
}

// capitalize_variable capitalizes the first letter for Erlang variables
pub fn (gen ErlangGenerator) capitalize_variable(name string) string {
	if name.len == 0 {
		return 'Var'
	}

	match name {
		'_' {
			return '_'
		}
		'__MODULE__' {
			return '?MODULE'
		}
		else {
			return name[0].str().to_upper() + name[1..]
		}
	}
}

// generate_function_call overrides base implementation for Erlang module calls
pub fn (gen ErlangGenerator) generate_function_call(call ast.FunctionCall) string {
	match call.function {
		ast.Identifier {
			// Check if it's a module call (e.g., math.pow)
			if call.function.name.contains('.') {
				parts := call.function.name.split('.')
				if parts.len == 2 {
					mod_name := parts[0]
					function := parts[1]
					args := call.arguments.map(gen.generate_expression(it, mut gen.context))
					return '${mod_name}:${function}(${args.join(', ')})'
				}
			}
		}
		else {}
	}

	// Fall back to base implementation
	return gen.BaseCodeGenerator.generate_function_call(call)
}

// generate_if overrides base implementation for Erlang if syntax
pub fn (gen ErlangGenerator) generate_if(if_expr ast.IfExpression) string {
	condition := gen.generate_expression(if_expr.condition, mut gen.context)
	then_body := gen.generate_expression(if_expr.then_body, mut gen.context)
	else_body := gen.generate_expression(if_expr.else_body, mut gen.context)

	// Erlang uses case for if expressions
	return 'case ${condition} of\ntrue ->\n${then_body};\nfalse ->\n${else_body}\nend'
}

// generate_with overrides base implementation for Erlang with syntax
pub fn (gen ErlangGenerator) generate_with(with_expr ast.WithExpression) string {
	mut result := 'case '

	for i, step in with_expr.steps {
		if i > 0 {
			result += ',\n'
		}
		pattern := gen.generate_pattern(step.pattern, mut gen.context)
		expression := gen.generate_expression(step.expression, mut gen.context)
		result += '${expression} of\n${pattern} ->'
	}

	result += '\n'
	gen.context.indent()
	result += gen.generate_expression(with_expr.body, mut gen.context)
	gen.context.dedent()
	result += '\nend'

	return result
}

// generate_for overrides base implementation for Erlang list comprehensions
pub fn (gen ErlangGenerator) generate_for(for_expr ast.ForExpression) string {
	pattern := gen.generate_pattern(for_expr.pattern, mut gen.context)
	collection := gen.generate_expression(for_expr.collection, mut gen.context)
	guard := if for_expr.guard != ast.Expression(ast.EmptyExpression{}) {
		', ' + gen.generate_expression(for_expr.guard, mut gen.context)
	} else {
		''
	}
	body := gen.generate_expression(for_expr.body, mut gen.context)

	return '[${body} || ${pattern} <- ${collection}${guard}]'
}

// generate_map overrides base implementation for Erlang map syntax
pub fn (gen ErlangGenerator) generate_map(map_lit ast.MapLiteral) string {
	mut pairs := []string{}

	for pair in map_lit.pairs {
		key := gen.generate_expression(pair.key, mut gen.context)
		value := gen.generate_expression(pair.value, mut gen.context)

		// Erlang maps use => for all keys
		pairs << '${key} => ${value}'
	}

	return '#{${pairs.join(', ')}}'
}

// generate_record overrides base implementation for Erlang record syntax
pub fn (gen ErlangGenerator) generate_record(record ast.RecordLiteral) string {
	record_name := record.name.to_lower()
	mut fields := []string{}

	for field in record.fields {
		value := gen.generate_expression(field.value, mut gen.context)
		fields << '${field.name} = ${value}'
	}

	return '#${record_name}{${fields.join(', ')}}'
}

// generate_fun overrides base implementation for Erlang fun syntax
pub fn (gen ErlangGenerator) generate_fun(fun_expr ast.FunExpression) string {
	mut clauses := []string{}

	for clause in fun_expr.clauses {
		params := clause.parameters.map(gen.generate_pattern(it, mut gen.context))
		guard := if clause.guard != ast.Expression(ast.EmptyExpression{}) {
			' when ' + gen.generate_expression(clause.guard, mut gen.context)
		} else {
			''
		}
		body := gen.generate_expression(clause.body, mut gen.context)

		clauses << '(${params.join(', ')})${guard} -> ${body}'
	}

	return 'fun ${clauses.join('; ')} end'
}

// generate_worker overrides base implementation for Erlang OTP workers
pub fn (gen ErlangGenerator) generate_worker(worker ast.Worker) string {
	mut result := '-module(${worker.name}).\n'
	result += '-behaviour(gen_server).\n\n'

	// Generate exports
	result += '-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).\n\n'

	// Generate start_link
	result += 'start_link() ->\n'
	result += '    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n\n'

	// Generate functions
	for function in worker.functions {
		result += gen.generate_function(function, mut gen.context)
		result += '\n'
	}

	return result
}

// generate_supervisor overrides base implementation for Erlang OTP supervisors
pub fn (gen ErlangGenerator) generate_supervisor(supervisor ast.Supervisor) string {
	mut result := '-module(${supervisor.name}).\n'
	result += '-behaviour(supervisor).\n\n'

	result += '-export([start_link/0, init/1]).\n\n'

	// Generate start_link
	result += 'start_link() ->\n'
	result += '    supervisor:start_link({local, ?MODULE}, ?MODULE, []).\n\n'

	// Generate init
	result += 'init([]) ->\n'
	result += '    Strategy = ${gen.translate_strategy(supervisor.strategy)},\n'
	result += '    Children = [${gen.generate_children(supervisor.children)}],\n'
	result += '    {ok, {Strategy, Children}}.\n'

	return result
}

// generate_record_definition overrides base implementation for Erlang records
pub fn (gen ErlangGenerator) generate_record_definition(record_def ast.RecordDefinition) string {
	record_name := record_def.name.to_lower()
	mut fields := []string{}

	for field in record_def.fields {
		fields << '${field.name}'
	}

	return '-record(${record_name}, {${fields.join(', ')}}).'
}

// generate_application overrides base implementation for Erlang applications
pub fn (gen ErlangGenerator) generate_application(app_def ast.ApplicationDefinition) string {
	mut result := '%% Application: ${app_def.name}\n'
	result += '%% Version: ${app_def.version}\n'
	result += '%% Description: ${app_def.description}\n'

	if app_def.modules.len > 0 {
		result += '%% Modules: ${app_def.modules.join(', ')}\n'
	}

	return result
}

// generate_test overrides base implementation for Erlang tests
pub fn (gen ErlangGenerator) generate_test(test_def ast.TestDefinition) string {
	return '%% Test: ${test_def.name}'
}

// generate_specification overrides base implementation for Erlang specs
pub fn (gen ErlangGenerator) generate_specification(spec_def ast.SpecificationDefinition) string {
	return '%% Specification: ${spec_def.name}'
}

// generate_dependency overrides base implementation for Erlang dependencies
pub fn (gen ErlangGenerator) generate_dependency(dep_def ast.DependencyDefinition) string {
	return '%% Dependency: ${dep_def.name}'
}

// get_file_extension returns the Erlang file extension
pub fn (gen ErlangGenerator) get_file_extension() string {
	return '.erl'
}

// get_module_header generates the Erlang module header
pub fn (gen ErlangGenerator) get_module_header(module_name string) string {
	return '-module(${module_name}).'
}

// get_module_footer generates the Erlang module footer
pub fn (gen ErlangGenerator) get_module_footer() string {
	return ''
}
