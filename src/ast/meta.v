module ast

pub struct Meta {
pub mut:
	start_line                 int
	end_line                   int
	start_pos                  int
	end_pos                    int
	literal                    Literal
	literal_accepts            []Literal // literal accepts for tuple or list
	kind                       MetaKind
	atom_attributes            ?AtomAttributes
	ident_attributes           ?IdentAttributes
	function_caller_attributes ?FunctionCallerAttributes
	function_attributes        ?FunctionAttributes
}

pub struct AtomAttributes {
pub:
	idx u32
}

pub struct IdentAttributes {
pub:
	idx u32
}

pub struct FunctionAttributes {
pub:
	idx       u32
	fun_table &FunTable
}

pub struct FunctionCallerAttributes {
pub:
	precedence u32
	position   MetaCallerFunctionPosition
}

pub fn FunctionCallerAttributes.new(prec u32, pos MetaCallerFunctionPosition) FunctionCallerAttributes {
	return FunctionCallerAttributes{prec, pos}
}

pub enum MetaCallerFunctionPosition {
	prefix
	infix
	postfix
}

pub enum MetaKind {
	k_comment
	k_literal
	k_keyword_list
	k_ident
	k_alias
	k_attribute
	k_function_def
	k_function_caller
	k_module_def
}

pub enum Literal {
	l_nil
	l_integer
	l_float
	l_boolean
	l_atom
	l_string
	l_list
	l_tuple
	l_char
	l_function
	l_any
}

pub fn (l Literal) str() string {
	return match l {
		.l_nil { 'nil' }
		.l_integer { 'integer' }
		.l_float { 'float' }
		.l_boolean { 'boolean' }
		.l_atom { 'atom' }
		.l_string { 'string' }
		.l_list { 'list' }
		.l_tuple { 'tuple' }
		.l_char { 'char' }
		.l_function { 'function' }
		.l_any { 'any' }
	}
}

pub fn Meta.new(line int, start_pos int) Meta {
	return Meta{
		start_line: line
		start_pos:  start_pos
	}
}

pub fn (m Meta) literal() Literal {
	return m.literal
}

pub fn (mut m Meta) set_literal(literal Literal) {
	m.literal = literal
}

pub fn (mut m Meta) set_kind(kind MetaKind) {
	m.kind = kind
}

pub fn (mut m Meta) set_literal_accepts(accepts []Literal) {
	m.literal_accepts = accepts
}

pub fn (mut m Meta) clear_attributes() {
	m.atom_attributes = none
	m.ident_attributes = none
	m.function_caller_attributes = none
}

pub fn (mut m Meta) set_atom_attributes(at AtomAttributes) {
	m.clear_attributes()
	m.atom_attributes = at
}

pub fn (mut m Meta) set_ident_attributes(idnt IdentAttributes) {
	m.clear_attributes()
	m.ident_attributes = idnt
}

pub fn (mut m Meta) set_function_caller_attributes(fc FunctionCallerAttributes) {
	m.clear_attributes()
	m.function_caller_attributes = fc
}

pub fn (mut m Meta) set_function_attributes(fc FunctionAttributes) {
	m.clear_attributes()
	m.function_attributes = fc
}

pub fn (mut m Meta) update_pos(end_line int, end_pos int) {
	m.end_pos = end_pos
	m.end_line = end_line
}

pub fn (mut m Meta) copy_literal_from_node(node Node) {
	lit := node.get_meta_literal()
	accepts := node.get_meta_literal_accepts()
	m.set_literal(lit)
	m.set_literal_accepts(accepts)
}
