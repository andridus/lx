module ast

pub fn (n Node) elixir(typ bool) string {
	sep := '::'
	mut kind := ''
	term := match n.left {
		Node {
			mut mapped := []string{}
			for n0 in n.nodes {
				mapped << n0.elixir(typ)
			}
			kind = n.kind.str()
			'{${n.left.elixir(typ)}, [], [${mapped.join(',')}]}'
		}
		Atom {
			kind = n.kind.str()
			if n.nodes.len > 0 {
				mut mapped := []string{}
				for n0 in n.nodes {
					mapped << n0.elixir(typ)
				}
				'{:${n.left.value.str()}, [], [${mapped.join(',')}]}'
			} else {
				':${n.left.value}'
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
				Charlist {
					'\'${n.left.str()}\''
				}
				Boolean {
					n.kind.value.str()
				}
				Module {
					'<<binary module ${n.kind.name.str()}>>'
				}
				Aliases {
					mut mapped := []string{}
					for n0 in n.nodes {
						mapped << n0.left as string
					}
					mapped.join('.')
				}
				List {
					if n.nodes.len > 0 {
						mut mapped := []string{}
						for n0 in n.nodes {
							mapped << n0.elixir(false)
						}
						'[${mapped.join(',')}]'
					} else {
						n.str()
					}
				}
				KeywordList {
					if n.nodes.len > 0 {
						mut mapped := []string{}
						for n0 in n.nodes {
							k := n0.nodes[0].elixir(false)
							v := n0.nodes[1].elixir(false)
							mapped << '${k.substr(1, k.len)}: ${v}'
						}
						'[${mapped.join(',')}]'
					} else {
						n.str()
					}
				}
				Tuple {
					if n.nodes.len > 0 {
						mut mapped := []string{}
						for n0 in n.nodes {
							mapped << n0.elixir(false)
						}
						'{${mapped.join(',')}}'
					} else {
						n.str()
					}
				}
				else {
					n.left.str()
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
