module ast

// node artifact
pub type NodeLeft = Atom | Node | f64 | int | string

pub fn (nd NodeLeft) to_str() string {
	return match nd {
		Atom { nd.value }
		Node { nd.left.to_str() }
		f64 { nd.str() }
		int { nd.str() }
		string { nd }
	}
}

fn str_to_node_left(value string, kind NodeKind) NodeLeft {
	return match kind {
		Integer { NodeLeft(value.int()) }
		Float { NodeLeft(value.f64()) }
		Function { NodeLeft(Atom{value}) }
		else { NodeLeft(value) }
	}
}
