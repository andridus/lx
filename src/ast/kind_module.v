module ast

pub struct Module {
	name string
	code Node
}

pub fn Module.new(name string, code Node) Module {
	return Module{
		name: name
		code: code
	}
}
