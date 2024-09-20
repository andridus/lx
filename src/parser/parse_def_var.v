module parser

import ast

fn (mut p Parser) parse_def_var() !ast.Node {
	mut meta := p.meta()
	ident := p.expect(._ident)!
	if p.in_args {
		meta.set_kind(.k_ident)
		meta.set_literal(p.parse_type()!)
		ident_val := p.var_table.insert(p.scope_context, ident.value(), meta.literal())!
		meta.set_ident_attributes(ast.IdentAttributes{ idx: ident_val })
		return ast.new_node(ident.value(), meta, none)
	} else if p.current_token.kind == ._attrb_op {
		p.expect(._attrb_op)!
		node := p.expr()!
		p.update_meta(mut meta)
		meta.copy_literal_from_node(node)

		ident_val := p.var_table.insert(p.scope_context, ident.value(), meta.literal())!
		meta.set_kind(.k_ident)
		meta.set_ident_attributes(ast.IdentAttributes{ idx: ident_val })
		ident_node := ast.new_node(ident.value(), meta, none)
		meta.set_kind(.k_function_caller)
		meta.set_atom_attributes(ast.AtomAttributes{ idx: 1 })
		return ast.Node{
			left:  '='
			nodes: [ident_node, node]
			meta:  meta
		}
	} else if p.current_token.kind == ._lpar {
		println('should be a function call')
		// verify if function was defined
		// verify args
		// create node
		return error('not a function')
	} else {
		if ident_val, lit := p.var_table.lookup_by_name(p.scope_context, ident.value()) {
			meta.set_literal(lit)
			meta.set_kind(.k_ident)
			meta.set_ident_attributes(ast.IdentAttributes{ident_val})
			return ast.new_node(ident.value(), meta, none)
		} else {
			return error('undefined variable `${ident.value()}`')
		}
	}
}

fn (mut p Parser) parse_type() !ast.Literal {
	if p.current_token.kind == ._type {
		p.expect(._type)!
		if _lit, typ := p.type_table.lookup_by_name(p.current_token.value()) {
			p.call_next_token() or {}
			return typ
		} else {
			node := p.expr()!
			if node.get_kind() != .k_ident {
				// define custom type here
			}
		}
	}
	return .l_nil
}
