module ast

pub fn (n Node) elixir(typ bool) string {
	sep := '::'
	mut kind := ''
	term := match n.left {
		Node {
			mut mapped := []string{}
			if nodes := n.nodes {
				for n0 in nodes {
					mapped << n0.elixir(typ)
				}
			}
			kind = n.kind.str()
			'{${n.left.elixir(typ)}, [], [${mapped.join(',')}]}'
		}
		Atom {
			kind = n.kind.str()
			if nodes := n.nodes {
				if nodes.len > 0 {
					mut mapped := []string{}
					for n0 in nodes {
						mapped << n0.elixir(typ)
					}
					'{:${n.left.value.str()}, [], [${mapped.join(',')}]}'
				} else {
					':${n.left.value}'
				}
			} else {
				'${n.left.value}'
			}
		}
		f64 {
			kind = n.kind.str()
			n.left.str()
		}
		int {
			kind = n.kind.str()
			n.left.str()
		}
		string {
			kind = n.kind.str()
			match n.kind {
				String {
					"\"${n.left.str()}"
				}
				Ident {
					'{${n.left.str()}, [], nil}'
				}
				Charlist {
					'\'${n.left.str()}\''
				}
				Boolean {
					n.kind.value.str()
				}
				Module {
					'<<binary module ${n.kind.name.str()}>>'
				}
				ModuleAttribute {
					if nodes := n.nodes {
						if nodes.len > 0 {
							mut mapped := []string{}
							for n0 in nodes {
								mapped << n0.elixir(false)
							}
							'{${mapped.join(',')}}'
						} else {
							n.str()
						}
					} else {
						n.str()
					}
				}
				Aliases {
					mut mapped := []string{}
					if nodes := n.nodes {
						for n0 in nodes {
							mapped << n0.left as string
						}
					}
					mapped.join('.')
				}
				List {
					if nodes := n.nodes {
						if nodes.len > 0 {
							mut mapped := []string{}
							for n0 in nodes {
								mapped << n0.elixir(false)
							}
							'[${mapped.join(',')}]'
						} else {
							n.str()
						}
					} else {
						n.str()
					}
				}
				KeywordList {
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
							'[${mapped.join(',')}]'
						} else {
							n.str()
						}
					} else {
						n.str()
					}
				}
				Tuple {
					if nodes := n.nodes {
						if nodes.len > 0 {
							mut mapped := []string{}
							for n0 in nodes {
								mapped << n0.elixir(false)
							}
							'{${mapped.join(',')}}'
						} else {
							n.str()
						}
					} else {
						n.left.str()
					}
				}
				else {
					'${n.kind}'
				}
			}
		}
	}
	if typ {
		return '${term}${sep}${kind}'
	} else {
		return '${term}'
	}
}
