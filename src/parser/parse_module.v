module parser

import ast

fn (mut p Parser) parse_module() !ast.Node {
	p.expect(._defmodule)!
	aliases := p.expect(._aliases)!
	module_name := aliases.value
	p.expect(._do)!
	// parse block here
	p.expect(._end)!
	p.call_next_token()!
	mod := ast.Module.new(module_name)
	return ast.new_node(mod)
}
