module generate

import ast
import typechecker.types
import context

// CodeGenerator defines the interface for all code generators
pub interface CodeGenerator {
	// generate_module generates code for an entire module
	generate_module(module ast.Module, type_ctx &types.Context) CodegenResult

	// generate_expression generates code for a single expression
	generate_expression(expr ast.Expression, mut ctx context.CodegenContext) string

	// generate_statement generates code for a single statement
	generate_statement(stmt ast.Statement, mut ctx context.CodegenContext) string

	// generate_function generates code for a function definition
	generate_function(func ast.Function, mut ctx context.CodegenContext) string

	// generate_pattern generates code for pattern matching
	generate_pattern(pattern ast.Pattern, mut ctx context.CodegenContext) string

	// get_file_extension returns the file extension for generated files
	get_file_extension() string

	// get_module_header generates the module header
	get_module_header(module_name string) string

	// get_module_footer generates the module footer
	get_module_footer() string
}

// CodegenResult represents the result of code generation
pub struct CodegenResult {
pub mut:
	success     bool
	code        string
	errors      []context.CodegenError
	warnings    []context.CodegenError
	module_name string
	file_path   string
}

// new_result creates a new code generation result
pub fn new_result(success bool, code string, module_name string, file_path string) CodegenResult {
	return CodegenResult{
		success:     success
		code:        code
		module_name: module_name
		file_path:   file_path
		errors:      []
		warnings:    []
	}
}

// BaseCodeGenerator provides common functionality for all generators
pub struct BaseCodeGenerator {
pub mut:
	context &context.CodegenContext = unsafe { nil }
}

// new_base_generator creates a new base generator
pub fn new_base_generator() BaseCodeGenerator {
	return BaseCodeGenerator{}
}

// set_context sets the generation context
pub fn (mut gen BaseCodeGenerator) set_context(ctx &context.CodegenContext) {
	gen.context = ctx
}

// generate_literal generates code for literal values
pub fn (gen BaseCodeGenerator) generate_literal(literal ast.Literal) string {
	match literal {
		ast.IntegerLiteral {
			return literal.value.str()
		}
		ast.FloatLiteral {
			return literal.value.str()
		}
		ast.StringLiteral {
			return '${literal.value}'
		}
		ast.BooleanLiteral {
			return if literal.value { 'true' } else { 'false' }
		}
		ast.AtomLiteral {
			return literal.value
		}
		ast.NilLiteral {
			return 'nil'
		}
	}
}

// generate_identifier generates code for identifiers
pub fn (gen BaseCodeGenerator) generate_identifier(ident ast.Identifier) string {
	if gen.context == unsafe { nil } {
		return ident.name
	}

	if variable := gen.context.find_variable(ident.name) {
		return variable.generated_name
	}

	// If not found in scope, generate a name
	return gen.context.generate_variable_name(ident.name)
}

// generate_binary_expression generates code for binary expressions
pub fn (gen BaseCodeGenerator) generate_binary_expression(expr ast.BinaryExpression) string {
	left := gen.generate_expression(expr.left, mut gen.context)
	right := gen.generate_expression(expr.right, mut gen.context)
	operator := gen.translate_operator(expr.operator)

	return '${left} ${operator} ${right}'
}

// translate_operator translates LX operators to target language operators
pub fn (gen BaseCodeGenerator) translate_operator(operator ast.BinaryOperator) string {
	match operator {
		.plus { return '+' }
		.minus { return '-' }
		.multiply { return '*' }
		.divide { return '/' }
		.equal { return '=:=' }
		.not_equal { return '=/=' }
		.less_than { return '<' }
		.greater_than { return '>' }
		.less_equal { return '=<' }
		.greater_equal { return '>=' }
		.and { return 'and' }
		.or { return 'or' }
		.andalso { return 'andalso' }
		.orelse { return 'orelse' }
		.concat { return '++' }
		.cons { return '|' }
	}
}

// generate_function_call generates code for function calls
pub fn (gen BaseCodeGenerator) generate_function_call(call ast.FunctionCall) string {
	function_name := gen.generate_expression(call.function, mut gen.context)
	args := call.arguments.map(gen.generate_expression(it, mut gen.context))

	return '${function_name}(${args.join(', ')})'
}

// generate_assignment generates code for assignments
pub fn (gen BaseCodeGenerator) generate_assignment(assign ast.Assignment) string {
	pattern := gen.generate_pattern(assign.pattern, mut gen.context)
	value := gen.generate_expression(assign.value, mut gen.context)

	return '${pattern} = ${value}'
}

// generate_block generates code for block expressions
pub fn (gen BaseCodeGenerator) generate_block(block ast.Block) string {
	if gen.context == unsafe { nil } {
		return ''
	}

	gen.context.enter_scope()

	mut result := ''
	for i, stmt in block.statements {
		if i > 0 {
			result += ',\n'
		}
		result += gen.generate_statement(stmt, mut gen.context)
	}

	gen.context.exit_scope()
	return result
}

// generate_case generates code for case expressions
pub fn (gen BaseCodeGenerator) generate_case(case_expr ast.CaseExpression) string {
	subject := gen.generate_expression(case_expr.subject, mut gen.context)

	mut result := 'case ${subject} of\n'
	gen.context.indent()

	for clause in case_expr.clauses {
		pattern := gen.generate_pattern(clause.pattern, mut gen.context)
		guard := if clause.guard != ast.Expression(ast.EmptyExpression{}) {
			' when ' + gen.generate_expression(clause.guard, mut gen.context)
		} else {
			''
		}
		body := gen.generate_expression(clause.body, mut gen.context)

		gen.context.writeln_indent('${pattern}${guard} ->')
		gen.context.indent()
		gen.context.writeln_indent(body)
		gen.context.dedent()
	}

	gen.context.dedent()
	gen.context.writeln_indent('end')

	return result
}

// generate_if generates code for if expressions
pub fn (gen BaseCodeGenerator) generate_if(if_expr ast.IfExpression) string {
	condition := gen.generate_expression(if_expr.condition, mut gen.context)
	then_body := gen.generate_expression(if_expr.then_body, mut gen.context)
	else_body := gen.generate_expression(if_expr.else_body, mut gen.context)

	return 'if ${condition} do\n${then_body}\nelse\n${else_body}\nend'
}

// generate_receive generates code for receive expressions
pub fn (gen BaseCodeGenerator) generate_receive(receive ast.ReceiveExpression) string {
	mut result := 'receive do\n'
	gen.context.indent()

	for clause in receive.clauses {
		pattern := gen.generate_pattern(clause.pattern, mut gen.context)
		guard := if clause.guard != ast.Expression(ast.EmptyExpression{}) {
			' when ' + gen.generate_expression(clause.guard, mut gen.context)
		} else {
			''
		}
		body := gen.generate_expression(clause.body, mut gen.context)

		gen.context.writeln_indent('${pattern}${guard} ->')
		gen.context.indent()
		gen.context.writeln_indent(body)
		gen.context.dedent()
	}

	if receive.timeout != ast.Expression(ast.EmptyExpression{}) {
		timeout := gen.generate_expression(receive.timeout, mut gen.context)
		timeout_body := gen.generate_expression(receive.timeout_body, mut gen.context)

		gen.context.writeln_indent('after ${timeout} do')
		gen.context.indent()
		gen.context.writeln_indent(timeout_body)
		gen.context.dedent()
	}

	gen.context.dedent()
	gen.context.writeln_indent('end')

	return result
}

// generate_with generates code for with expressions
pub fn (gen BaseCodeGenerator) generate_with(with_expr ast.WithExpression) string {
	mut result := 'with '

	for i, step in with_expr.steps {
		if i > 0 {
			result += ',\n'
		}
		pattern := gen.generate_pattern(step.pattern, mut gen.context)
		expression := gen.generate_expression(step.expression, mut gen.context)
		result += '${pattern} <= ${expression}'
	}

	result += ' do\n'
	gen.context.indent()
	result += gen.generate_expression(with_expr.body, mut gen.context)
	gen.context.dedent()
	result += '\nend'

	return result
}

// generate_for generates code for for expressions
pub fn (gen BaseCodeGenerator) generate_for(for_expr ast.ForExpression) string {
	pattern := gen.generate_pattern(for_expr.pattern, mut gen.context)
	collection := gen.generate_expression(for_expr.collection, mut gen.context)
	guard := if for_expr.guard != ast.Expression(ast.EmptyExpression{}) {
		' when ' + gen.generate_expression(for_expr.guard, mut gen.context)
	} else {
		''
	}
	body := gen.generate_expression(for_expr.body, mut gen.context)

	return '[${body} || ${pattern} <- ${collection}${guard}]'
}

// generate_list generates code for list literals
pub fn (gen BaseCodeGenerator) generate_list(list ast.ListLiteral) string {
	elements := list.elements.map(gen.generate_expression(it, mut gen.context))
	return '[${elements.join(', ')}]'
}

// generate_tuple generates code for tuple literals
pub fn (gen BaseCodeGenerator) generate_tuple(tuple ast.TupleLiteral) string {
	elements := tuple.elements.map(gen.generate_expression(it, mut gen.context))
	return '{${elements.join(', ')}}'
}

// generate_map generates code for map literals
pub fn (gen BaseCodeGenerator) generate_map(map_lit ast.MapLiteral) string {
	mut pairs := []string{}

	for pair in map_lit.pairs {
		key := gen.generate_expression(pair.key, mut gen.context)
		value := gen.generate_expression(pair.value, mut gen.context)

		// Determine if key is atom or general
		key_str := match pair.key {
			ast.AtomLiteral { key }
			ast.StringLiteral { '${key}' }
			else { key }
		}

		pairs << '${key_str} => ${value}'
	}

	return '#{${pairs.join(', ')}}'
}

// generate_record generates code for record literals
pub fn (gen BaseCodeGenerator) generate_record(record ast.RecordLiteral) string {
	record_name := record.name.to_lower()
	mut fields := []string{}

	for field in record.fields {
		value := gen.generate_expression(field.value, mut gen.context)
		fields << '${field.name}: ${value}'
	}

	return '#${record_name}{${fields.join(', ')}}'
}

// generate_binary generates code for binary literals
pub fn (gen BaseCodeGenerator) generate_binary(binary ast.BinaryLiteral) string {
	mut segments := []string{}

	for segment in binary.segments {
		value := gen.generate_expression(segment.value, mut gen.context)
		size := if segment.size != ast.Expression(ast.EmptyExpression{}) {
			':' + gen.generate_expression(segment.size, mut gen.context)
		} else {
			''
		}
		type_spec := if segment.type_spec != '' {
			'/${segment.type_spec}'
		} else {
			''
		}

		segments << '${value}${size}${type_spec}'
	}

	return '<<${segments.join(', ')}>>'
}

// generate_fun generates code for fun expressions
pub fn (gen BaseCodeGenerator) generate_fun(fun_expr ast.FunExpression) string {
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

// generate_message_send generates code for message sending
pub fn (gen BaseCodeGenerator) generate_message_send(send ast.MessageSend) string {
	pid := gen.generate_expression(send.pid, mut gen.context)
	message := gen.generate_expression(send.message, mut gen.context)

	return '${pid} ! ${message}'
}

// generate_worker generates code for worker definitions
pub fn (gen BaseCodeGenerator) generate_worker(worker ast.Worker) string {
	mut result := '-module(${worker.name}).\n'
	result += '-behaviour(gen_server).\n\n'

	// Generate exports
	result += '-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).\n\n'

	// Generate functions
	for function in worker.functions {
		result += gen.generate_function(function, mut gen.context)
		result += '\n'
	}

	return result
}

// generate_supervisor generates code for supervisor definitions
pub fn (gen BaseCodeGenerator) generate_supervisor(supervisor ast.Supervisor) string {
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

// translate_strategy translates LX strategy to Erlang strategy
pub fn (gen BaseCodeGenerator) translate_strategy(strategy ast.SupervisorStrategy) string {
	match strategy {
		.one_for_one { return 'one_for_one' }
		.one_for_all { return 'one_for_all' }
		.rest_for_one { return 'rest_for_one' }
	}
}

// generate_children generates code for supervisor children
pub fn (gen BaseCodeGenerator) generate_children(children []ast.SupervisorChild) string {
	mut child_specs := []string{}

	for child in children {
		name := child.name
		mod_name := child.module
		args := child.arguments.map(gen.generate_expression(it, mut gen.context))

		child_specs << '{${name}, {${mod_name}, start_link, [${args.join(', ')}]}, permanent, 5000, worker, [${mod_name}]}'
	}

	return child_specs.join(',\n        ')
}

// Default implementations for interface methods
pub fn (gen BaseCodeGenerator) generate_expression(expr ast.Expression, mut ctx context.CodegenContext) string {
	gen.set_context(ctx)

	match expr {
		ast.Literal { return gen.generate_literal(expr) }
		ast.Identifier { return gen.generate_identifier(expr) }
		ast.BinaryExpression { return gen.generate_binary_expression(expr) }
		ast.FunctionCall { return gen.generate_function_call(expr) }
		ast.Assignment { return gen.generate_assignment(expr) }
		ast.Block { return gen.generate_block(expr) }
		ast.CaseExpression { return gen.generate_case(expr) }
		ast.IfExpression { return gen.generate_if(expr) }
		ast.ReceiveExpression { return gen.generate_receive(expr) }
		ast.WithExpression { return gen.generate_with(expr) }
		ast.ForExpression { return gen.generate_for(expr) }
		ast.ListLiteral { return gen.generate_list(expr) }
		ast.TupleLiteral { return gen.generate_tuple(expr) }
		ast.MapLiteral { return gen.generate_map(expr) }
		ast.RecordLiteral { return gen.generate_record(expr) }
		ast.BinaryLiteral { return gen.generate_binary(expr) }
		ast.FunExpression { return gen.generate_fun(expr) }
		ast.MessageSend { return gen.generate_message_send(expr) }
		ast.EmptyExpression { return '' }
	}
}

pub fn (gen BaseCodeGenerator) generate_statement(stmt ast.Statement, mut ctx context.CodegenContext) string {
	gen.set_context(ctx)

	match stmt {
		ast.ExpressionStatement { return gen.generate_expression(stmt.expression, mut
				ctx) }
		ast.FunctionDefinition { return gen.generate_function(stmt.function, mut ctx) }
		ast.RecordDefinition { return gen.generate_record_definition(stmt) }
		ast.WorkerDefinition { return gen.generate_worker(stmt.worker) }
		ast.SupervisorDefinition { return gen.generate_supervisor(stmt.supervisor) }
		ast.ApplicationDefinition { return gen.generate_application(stmt) }
		ast.TestDefinition { return gen.generate_test(stmt) }
		ast.SpecificationDefinition { return gen.generate_specification(stmt) }
		ast.DependencyDefinition { return gen.generate_dependency(stmt) }
	}
}

pub fn (gen BaseCodeGenerator) generate_pattern(pattern ast.Pattern, mut ctx context.CodegenContext) string {
	gen.set_context(ctx)

	match pattern {
		ast.LiteralPattern { return gen.generate_literal(pattern.literal) }
		ast.IdentifierPattern { return gen.generate_identifier(pattern.identifier) }
		ast.TuplePattern { return gen.generate_tuple_pattern(pattern) }
		ast.ListPattern { return gen.generate_list_pattern(pattern) }
		ast.MapPattern { return gen.generate_map_pattern(pattern) }
		ast.RecordPattern { return gen.generate_record_pattern(pattern) }
		ast.BinaryPattern { return gen.generate_binary_pattern(pattern) }
		ast.WildcardPattern { return '_' }
	}
}

pub fn (gen BaseCodeGenerator) generate_function(func ast.Function, mut ctx context.CodegenContext) string {
	gen.set_context(ctx)

	mut result := ''

	for clause in func.clauses {
		name := func.name
		params := clause.parameters.map(gen.generate_pattern(it, mut ctx))
		guard := if clause.guard != ast.Expression(ast.EmptyExpression{}) {
			' when ' + gen.generate_expression(clause.guard, mut ctx)
		} else {
			''
		}
		body := gen.generate_expression(clause.body, mut ctx)

		result += '${name}(${params.join(', ')})${guard} ->\n'
		ctx.indent()
		result += ctx.write_indent() + body + '.\n'
		ctx.dedent()
	}

	return result
}

// Placeholder implementations for remaining methods
pub fn (gen BaseCodeGenerator) generate_record_definition(record_def ast.RecordDefinition) string {
	return '%% Record definition: ${record_def.name}'
}

pub fn (gen BaseCodeGenerator) generate_application(app_def ast.ApplicationDefinition) string {
	return '%% Application definition: ${app_def.name}'
}

pub fn (gen BaseCodeGenerator) generate_test(test_def ast.TestDefinition) string {
	return '%% Test definition: ${test_def.name}'
}

pub fn (gen BaseCodeGenerator) generate_specification(spec_def ast.SpecificationDefinition) string {
	return '%% Specification definition: ${spec_def.name}'
}

pub fn (gen BaseCodeGenerator) generate_dependency(dep_def ast.DependencyDefinition) string {
	return '%% Dependency definition: ${dep_def.name}'
}

pub fn (gen BaseCodeGenerator) generate_tuple_pattern(pattern ast.TuplePattern) string {
	elements := pattern.elements.map(gen.generate_pattern(it, mut gen.context))
	return '{${elements.join(', ')}}'
}

pub fn (gen BaseCodeGenerator) generate_list_pattern(pattern ast.ListPattern) string {
	elements := pattern.elements.map(gen.generate_pattern(it, mut gen.context))
	return '[${elements.join(', ')}]'
}

pub fn (gen BaseCodeGenerator) generate_map_pattern(pattern ast.MapPattern) string {
	mut pairs := []string{}

	for pair in pattern.pairs {
		key := gen.generate_pattern(pair.key, mut gen.context)
		value := gen.generate_pattern(pair.value, mut gen.context)
		pairs << '${key} := ${value}'
	}

	return '#{${pairs.join(', ')}}'
}

pub fn (gen BaseCodeGenerator) generate_record_pattern(pattern ast.RecordPattern) string {
	record_name := pattern.name.to_lower()
	mut fields := []string{}

	for field in pattern.fields {
		value := gen.generate_pattern(field.value, mut gen.context)
		fields << '${field.name}: ${value}'
	}

	return '#${record_name}{${fields.join(', ')}}'
}

pub fn (gen BaseCodeGenerator) generate_binary_pattern(pattern ast.BinaryPattern) string {
	mut segments := []string{}

	for segment in pattern.segments {
		value := gen.generate_pattern(segment.value, mut gen.context)
		size := if segment.size != ast.Expression(ast.EmptyExpression{}) {
			':' + gen.generate_expression(segment.size, mut gen.context)
		} else {
			''
		}
		type_spec := if segment.type_spec != '' {
			'/${segment.type_spec}'
		} else {
			''
		}

		segments << '${value}${size}${type_spec}'
	}

	return '<<${segments.join(', ')}>>'
}

pub fn (gen BaseCodeGenerator) get_file_extension() string {
	return '.erl'
}

pub fn (gen BaseCodeGenerator) get_module_header(module_name string) string {
	return '-module(${module_name}).\n'
}

pub fn (gen BaseCodeGenerator) get_module_footer() string {
	return ''
}
