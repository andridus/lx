module erlang

import ast
import typechecker

// ErlangGenerator generates Erlang code from LX AST
pub struct ErlangGenerator {
}

// new_erlang_generator creates a new Erlang code generator
pub fn new_erlang_generator() ErlangGenerator {
	return ErlangGenerator{}
}

// CodegenResult represents the result of code generation
pub struct CodegenResult {
pub:
	success bool
	code    string
	errors  []CodegenError
}

// CodegenError represents an error during code generation
pub struct CodegenError {
pub:
	message string
}

// generate_module generates a complete Erlang module
pub fn (gen ErlangGenerator) generate_module(mod ast.ModuleStmt, type_ctx &typechecker.TypeContext) CodegenResult {
	mut code := ''

	// Generate module header
	code += '-module(${mod.name}).\n'

	// Generate exports if there are functions
	if mod.statements.len > 0 {
		exports := gen.generate_exports(mod.statements)
		if exports.len > 0 {
			code += '-export([${exports.join(', ')}]).\n\n'
		}
	}

	// Generate each statement
	for stmt in mod.statements {
		generated := gen.generate_statement(stmt)
		if generated.len > 0 {
			code += generated + '\n\n'
		}
	}

	return CodegenResult{
		success: true
		code:    code
		errors:  []
	}
}

// generate_exports generates export declarations for functions
pub fn (gen ErlangGenerator) generate_exports(statements []ast.Stmt) []string {
	mut exports := []string{}

	for stmt in statements {
		match stmt {
			ast.FunctionStmt {
				function_name := stmt.name
				if stmt.clauses.len > 0 {
					arity := stmt.clauses[0].parameters.len
					exports << '${function_name}/${arity}'
				}
			}
			else {}
		}
	}

	return exports
}

// generate_statement generates code for a single statement
pub fn (gen ErlangGenerator) generate_statement(stmt ast.Stmt) string {
	match stmt {
		ast.ExprStmt {
			return gen.generate_expression(stmt.expr)
		}
		ast.FunctionStmt {
			return gen.generate_function(stmt)
		}
		ast.ModuleStmt {
			// Module statements are handled at the top level
			return ''
		}
		ast.RecordDefStmt {
			return gen.generate_record_definition(stmt)
		}
		ast.TypeDefStmt {
			return gen.generate_type_definition(stmt)
		}
	}
}

// generate_expression generates code for a single expression
pub fn (gen ErlangGenerator) generate_expression(expr ast.Expr) string {
	match expr {
		ast.VariableExpr {
			return gen.generate_variable(expr.name)
		}
		ast.LiteralExpr {
			return gen.generate_literal(expr.value)
		}
		ast.AssignExpr {
			return gen.generate_assignment(expr)
		}
		ast.BinaryExpr {
			return gen.generate_binary_expression(expr)
		}
		ast.CallExpr {
			return gen.generate_function_call(expr)
		}
		ast.MatchExpr {
			return gen.generate_match(expr)
		}
		ast.ListConsExpr {
			return gen.generate_list_cons(expr)
		}
		ast.ListEmptyExpr {
			return '[]'
		}
		ast.ListLiteralExpr {
			return gen.generate_list_literal(expr)
		}
		ast.TupleExpr {
			return gen.generate_tuple(expr)
		}
		ast.MapLiteralExpr {
			return gen.generate_map_literal(expr)
		}
		ast.RecordLiteralExpr {
			return gen.generate_record_literal(expr)
		}
		ast.RecordAccessExpr {
			return gen.generate_record_access(expr)
		}
		ast.FunExpr {
			return gen.generate_fun_expression(expr)
		}
		ast.SendExpr {
			return gen.generate_send(expr)
		}
		ast.ReceiveExpr {
			return gen.generate_receive(expr)
		}
		ast.GuardExpr {
			return gen.generate_guard(expr)
		}
	}
}

// generate_variable generates code for a variable
pub fn (gen ErlangGenerator) generate_variable(name string) string {
	return gen.capitalize_variable(name)
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

// generate_literal generates code for literals
pub fn (gen ErlangGenerator) generate_literal(literal ast.Literal) string {
	match literal {
		ast.StringLiteral {
			// Escape quotes and special characters for Erlang
			escaped := literal.value.replace('\\', '\\\\').replace('"', '\\"')
			return "\"${escaped}\""
		}
		ast.IntegerLiteral {
			return literal.value.str()
		}
		ast.FloatLiteral {
			return literal.value.str()
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

// generate_assignment generates code for assignments
pub fn (gen ErlangGenerator) generate_assignment(assign ast.AssignExpr) string {
	value := gen.generate_expression(assign.value)
	return '${gen.capitalize_variable(assign.name)} = ${value}'
}

// generate_binary_expression generates code for binary expressions
pub fn (gen ErlangGenerator) generate_binary_expression(expr ast.BinaryExpr) string {
	left := gen.generate_expression(expr.left)
	right := gen.generate_expression(expr.right)
	op := gen.translate_operator(expr.op)
	return '${left} ${op} ${right}'
}

// translate_operator translates LX operators to Erlang operators
pub fn (gen ErlangGenerator) translate_operator(op ast.BinaryOp) string {
	return op.str()
}

// generate_function_call generates code for function calls
pub fn (gen ErlangGenerator) generate_function_call(call ast.CallExpr) string {
	function := gen.generate_expression(call.function)
	args := call.arguments.map(gen.generate_expression(it))
	return '${function}(${args.join(', ')})'
}

// generate_match generates code for pattern matching
pub fn (gen ErlangGenerator) generate_match(match_expr ast.MatchExpr) string {
	value := gen.generate_expression(match_expr.value)
	mut cases := []string{}

	for case_item in match_expr.cases {
		pattern := gen.generate_pattern(case_item.pattern)
		guard := if case_item.guard != ast.Expr(ast.GuardExpr{}) {
			' when ' + gen.generate_expression(case_item.guard)
		} else {
			''
		}
		body := case_item.body.map(gen.generate_statement(it))
		cases << '${pattern}${guard} ->\n${body.join(';\n')}'
	}

	return 'case ${value} of\n${cases.join(';\n')}\nend'
}

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

// generate_list_cons generates code for list cons
pub fn (gen ErlangGenerator) generate_list_cons(expr ast.ListConsExpr) string {
	head := gen.generate_expression(expr.head)
	tail := gen.generate_expression(expr.tail)
	return '[${head} | ${tail}]'
}

// generate_list_literal generates code for list literals
pub fn (gen ErlangGenerator) generate_list_literal(expr ast.ListLiteralExpr) string {
	elements := expr.elements.map(gen.generate_expression(it))
	return '[${elements.join(', ')}]'
}

// generate_tuple generates code for tuples
pub fn (gen ErlangGenerator) generate_tuple(expr ast.TupleExpr) string {
	elements := expr.elements.map(gen.generate_expression(it))
	return '{${elements.join(', ')}}'
}

// generate_map_literal generates code for map literals
pub fn (gen ErlangGenerator) generate_map_literal(expr ast.MapLiteralExpr) string {
	mut entries := []string{}
	for entry in expr.entries {
		key := gen.generate_expression(entry.key)
		value := gen.generate_expression(entry.value)
		entries << '${key} => ${value}'
	}
	return '#{${entries.join(', ')}}'
}

// generate_record_literal generates code for record literals
pub fn (gen ErlangGenerator) generate_record_literal(expr ast.RecordLiteralExpr) string {
	mut fields := []string{}
	for field in expr.fields {
		value := gen.generate_expression(field.value)
		fields << '${field.name}: ${value}'
	}
	return '#${expr.name}{${fields.join(', ')}}'
}

// generate_record_access generates code for record access
pub fn (gen ErlangGenerator) generate_record_access(expr ast.RecordAccessExpr) string {
	record := gen.generate_expression(expr.record)
	return '${record}#${expr.field}'
}

// generate_fun_expression generates code for fun expressions
pub fn (gen ErlangGenerator) generate_fun_expression(expr ast.FunExpr) string {
	mut clauses := []string{}
	for clause in expr.parameters {
		pattern := gen.generate_pattern(clause)
		clauses << pattern
	}
	body := expr.body.map(gen.generate_statement(it))
	return 'fun(${clauses.join(', ')}) ->\n${body.join(';\n')}\nend'
}

// generate_send generates code for message sending
pub fn (gen ErlangGenerator) generate_send(expr ast.SendExpr) string {
	pid := gen.generate_expression(expr.pid)
	message := gen.generate_expression(expr.message)
	return '${pid} ! ${message}'
}

// generate_receive generates code for receive expressions
pub fn (gen ErlangGenerator) generate_receive(expr ast.ReceiveExpr) string {
	mut cases := []string{}
	for case_item in expr.cases {
		pattern := gen.generate_pattern(case_item.pattern)
		guard := if case_item.guard != ast.Expr(ast.GuardExpr{}) {
			' when ' + gen.generate_expression(case_item.guard)
		} else {
			''
		}
		body := case_item.body.map(gen.generate_statement(it))
		cases << '${pattern}${guard} ->\n${body.join(';\n')}'
	}

	timeout := if expr.timeout != ast.Expr(ast.GuardExpr{}) {
		' after ${gen.generate_expression(expr.timeout)} ->\ntimeout'
	} else {
		''
	}

	return 'receive\n${cases.join(';\n')}${timeout}\nend'
}

// generate_guard generates code for guard expressions
pub fn (gen ErlangGenerator) generate_guard(expr ast.GuardExpr) string {
	return gen.generate_expression(expr.condition)
}

// generate_function generates code for function definitions
pub fn (gen ErlangGenerator) generate_function(func ast.FunctionStmt) string {
	mut clauses := []string{}

	for clause in func.clauses {
		parameters := clause.parameters.map(gen.generate_pattern(it))
		guard := if clause.guard != ast.Expr(ast.GuardExpr{}) {
			' when ' + gen.generate_expression(clause.guard)
		} else {
			''
		}
		body := clause.body.map(gen.generate_statement(it))
		clauses << '${func.name}(${parameters.join(', ')})${guard} ->\n${body.join(';\n')}'
	}

	return clauses.join(';\n') + '.'
}

// generate_record_definition generates code for record definitions
pub fn (gen ErlangGenerator) generate_record_definition(record_def ast.RecordDefStmt) string {
	mut fields := []string{}
	for field in record_def.fields {
		fields << '${field.name}'
	}
	return '-record(${record_def.name}, {${fields.join(', ')}}).'
}

// generate_type_definition generates code for type definitions
pub fn (gen ErlangGenerator) generate_type_definition(type_def ast.TypeDefStmt) string {
	return '%% Type definition: ${type_def.name}'
}