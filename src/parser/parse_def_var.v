module parser

import ast

fn (mut p Parser) parse_def_var() !ast.Node {
	ident := p.expect(._ident)!
	if p.current_token.kind == ._attrb_op {
		p.expect(._attrb_op)!
		node := p.expr()!
		ident_val := p.var_table.insert(ident.value())!
		return ast.new_def_var_node(ident_val, ident.value(), node)
	}
	if ident_val := p.var_table.lookup_by_name(ident.value()) {
		return ast.new_node_2(ident.value(), ast.Ident.new(ident_val))
	} else {
		return error('undefined variable `${ident.value()}`')
	}
}
