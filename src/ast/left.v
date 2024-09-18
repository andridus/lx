module ast

// node artifact
pub type NodeLeft = Node | f64 | int | string

pub fn (nd NodeLeft) to_str() string {
	return match nd {
		Node { nd.left.to_str() }
		f64 { nd.str() }
		int { nd.str() }
		string { nd }
	}
}
