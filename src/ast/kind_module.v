module ast

pub struct Module {
	name       string
	attributes map[string]Node
	code       []Node
}

pub fn Module.new(name string, attributes map[string]Node, code []Node) Module {
	return Module{
		name:       name
		attributes: attributes
		code:       code
	}
}
