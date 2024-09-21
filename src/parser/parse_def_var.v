module parser

import ast

fn (mut p Parser) parse_def_var() !ast.Node {
	mut meta := p.meta()
	ident := p.expect(._ident)!
	if p.in_args {
		meta.set_kind(.k_ident)
		if p.current_token.kind == ._type {
			meta.set_literal(p.parse_type()!)
		} else {
			meta.set_literal(.l_any)
		}
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
		mut args_types := []ast.Literal{}
		mut args_nodes := []ast.Node{}
		p.in_args = true
		p.expect(._lpar)!
		p.ignore_next_newline()
		for {
			if p.current_token.kind == ._rpar {
				p.expect(._rpar)!
				break
			}
			node := p.expr()!
			args_types << node.get_meta_literal()
			args_nodes << node
			ex0 := p.expect_one_of([._rpar, ._comma])!
			match ex0.kind {
				._comma {
					continue
				}
				._rpar {
					break
				}
				else {}
			}
		}
		p.in_args = false
		fun_name := '${ident.value()}/${args_types.len}'
		if idx, ret , args:= p.fun_table.lookup_by_name(fun_name) {
			if args_idx := ast.find_args_idx(args_types, args, true) {
				mut returns := ret[args_idx]
				p.update_meta(mut meta)
				if returns == .l_any && ast.Literal.l_any in args[args_idx] {
					returns = p.fun_table.reevaluate_with_args(idx, args_idx, args[args_idx], args_types)
				}
				meta.set_literal(returns)
				meta.set_kind(.k_function_caller)
				meta.set_function_caller_attributes(ast.FunctionCallerAttributes{
					args_idx: args_idx
					idx: idx
				})
				line, pos := p.lexer.current_position()
				p.fun_table.insert_caller(idx, args_idx, p.current_module.join('.'), p.filepath, u32(pos), u32(line))
				return ast.new_node(ident.value(), meta, args_nodes)
			}
		}
		fun_name0 := '${ident.value()}(${args_types.map(it.str()).join(', ')})'
		return error('undefined function `${fun_name0}`')
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
