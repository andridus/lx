module ast

pub fn (n Node) elixir() string {
	return match n.left {
		Node {
			mut mapped := []string{}
			for n0 in n.nodes {
				mapped << n0.elixir()
			}
			'{${n.left.elixir()}, [], [${mapped.join(',')}]}'
		}
		Atom {
			if n.nodes.len > 0 {
				mut mapped := []string{}
				for n0 in n.nodes {
					mapped << n0.elixir()
				}
				'{:${n.left.value.str()}, [], [${mapped.join(',')}]}'
			} else {
				n.left.value
			}
		}
		f64 {
			n.left.str()
		}
		int {
			n.left.str()
		}
		string {
			match n.kind {
				String { "\"${n.left.str()}\"" }
				Charlist { '\'${n.left.str()}\'' }
				Boolean { n.kind.value.str() }
				else { n.left.str() }
			}
		}
	}
}
