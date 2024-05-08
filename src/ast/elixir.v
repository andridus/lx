module ast

pub fn (n Node) elixir() string {
	return match n.left {
		Node {
			mut mapped := []string{}
			for n0 in n.nodes {
				mapped << n0.elixir()
			}
			'{${n.left.str()}, [], [${mapped.join(',')}]}'
		}
		f64 {
			n.left.str()
		}
		int {
			n.left.str()
		}
		string {
			n.left.str()
		}
	}
}
