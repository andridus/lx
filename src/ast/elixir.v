module ast

pub fn (n Node) elixir(typ bool) string {
	sep := '::'
	mut kind := ''
	mut term := '-'
	match n.meta.kind {
		.k_comment {}
		.k_literal {
			match n.meta.literal {
				.l_nil {
					kind = 'nil'
					term = n.left.to_str()
				}
				.l_integer {
					kind = 'integer'
					term = n.left.to_str()
				}
				.l_float {
					kind = 'float'
					term = n.left.to_str()
				}
				.l_boolean {
					kind = 'boolean'
					term = n.left.to_str()
				}
				.l_atom {
					kind = 'atom'
					// val := n.left as Atom
					// if n.meta.has_atom_delimiter {
					// 	term = ':\"${val.value}\"'
					// } else {
					term = ':${n.left}'
					// }
				}
				.l_string {
					kind = 'string'
					val := n.left as string
					term = "\"${val}\""
				}
				.l_list {
					mut kinds := []string{}
					for la in n.meta.literal_accepts {
						kinds << la.str()
					}
					kind = 'list(${kinds.join(', ')})'
					term = '[${n.left.to_str()}]'
					if nodes := n.nodes {
						if nodes.len > 0 {
							mut mapped := []string{}
							for n0 in nodes {
								mapped << n0.elixir(false)
							}
							term = '[${mapped.join(',')}]'
						}
					}
				}
				.l_tuple {
					mut kinds := []string{}
					for la in n.meta.literal_accepts {
						kinds << la.str()
					}
					kind = 'tuple(${kinds.join(', ')})'
					term = '{${n.left.to_str()}}'
					if nodes := n.nodes {
						if nodes.len > 0 {
							mut mapped := []string{}
							for n0 in nodes {
								mapped << n0.elixir(false)
							}
							term = '{${mapped.join(',')}}'
						}
					}
				}
				.l_char {}
				.l_function {}
				.l_any {}
			}
		}
		.k_keyword_list {
			kind = 'keyword_list'
			term = '[]'
			if nodes := n.nodes {
				if nodes.len > 0 {
					mut mapped := []string{}
					for n0 in nodes {
						if nodes0 := n0.nodes {
							k := nodes0[0].elixir(false)
							v := nodes0[1].elixir(false)
							mapped << '${k.substr(1, k.len)}: ${v}'
						}
					}
					term = '[${mapped.join(',')}]'
				}
			}
		}
		.k_ident {
			kind = n.meta.literal().str()
			term = '{${n.left.to_str()}, [], nil}'
		}
		.k_alias {
			kind = 'alias'
			mut mapped := []string{}
			if nodes := n.nodes {
				for n0 in nodes {
					mapped << n0.left as string
				}
			}
			term = mapped.join('.')
		}
		.k_attribute {
			kind = 'attribute'
			if nodes := n.nodes {
				key := nodes[0].left.to_str()
				value := nodes[1].elixir(typ)
				term = '{:@, [], [${key}, ${value}]}'
			}
		}
		.k_function_def {}
		.k_function_caller {
			kind = 'function_caller'
			if nodes := n.nodes {
				if nodes.len > 0 {
					mut mapped := []string{}
					for n0 in nodes {
						mapped << n0.elixir(false)
					}
					term = '{:${n.left.to_str()}, [], [${mapped.join(',')}]}'
				}
			}
		}
		.k_module_def {
			kind = 'module'
			mut nodes_ex := []string{}
			if nodes := n.nodes {
				for n0 in nodes {
					nodes_ex << n0.elixir(typ)
				}
			}
			term = '{:module, ${n.left.to_str()}, [${nodes_ex.join(',')}]}'
		}
	}
	if typ {
		return '${term}${sep}${kind}'
	} else {
		return '${term}'
	}
}
