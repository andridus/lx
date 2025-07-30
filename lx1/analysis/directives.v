module analysis

import ast

pub type DirectiveHandler = fn (args []ast.Node, analyzer &Analyzer) !

pub struct DirectiveInfo {
pub:
	name           string
	description    string
	argument_count int
	handler        ?DirectiveHandler
}

// Directives registry
pub const directives = {
	'print': DirectiveInfo{
		name:           'print'
		description:    'Prints the AST of the argument using V println'
		argument_count: 1
		handler:        print_directive_handler
	}
	'type':  DirectiveInfo{
		name:           'type'
		description:    'Shows the inferred type of the argument'
		argument_count: 1
		handler:        type_directive_handler
	}
}

pub fn get_directive_info(name string) ?DirectiveInfo {
	return directives[name] or { return none }
}

pub fn is_valid_directive(name string) bool {
	return name in directives
}

fn print_directive_handler(args []ast.Node, analyzer &Analyzer) ! {
	if args.len != 1 {
		return error('print directive requires exactly 1 argument')
	}

	arg := args[0]
	println('${arg.position}:')
	println(arg.tree_str(0))
}

fn type_directive_handler(args []ast.Node, analyzer &Analyzer) ! {
	if args.len != 1 {
		return error('type directive requires exactly 1 argument')
	}

	arg := args[0]

	arg_type := analyzer.type_table.get_type(arg.id) or {
		println('Type: unknown')
		return
	}

	println('${args[0].kind}(${args[0].value}): ${arg_type.str()}')
}
