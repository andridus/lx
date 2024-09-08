module parser

import ast

fn (mut p Parser) parse_module() !ast.Node {
	mut code := ast.Node{}
	p.expect(._defmodule)!
	aliases := p.expect(._aliases)!
	module_name := aliases.value

	if p.current_token.kind == ._comma {
		p.expect(._comma)!
		p.expect_keyword('do')!
		code = p.expr()!
	} else {
		p.expect(._do)!
		if p.current_token.kind != ._end {
			code = p.stmt()!
		}
		p.expect(._end)!
	}
	mod := ast.Module.new(module_name, code)
	return ast.new_node(mod)
}