module ast

// VariableExpr represents a variable reference
pub struct VariableExpr {
pub:
	name     string
	position Position
}

// LiteralExpr represents a literal expression
pub struct LiteralExpr {
pub:
	value    Literal
	position Position
}

// AssignExpr represents an assignment expression
pub struct AssignExpr {
pub:
	name            string
	value           Expr
	type_annotation ?TypeExpression
	position        Position
}

// BinaryExpr represents a binary operation
pub struct BinaryExpr {
pub:
	left     Expr
	op       BinaryOp
	right    Expr
	position Position
}

// CallExpr represents a function call
pub struct CallExpr {
pub:
	function      Expr   // used for internal calls
	external      bool   // true if this is an external call
	module        string // module name if external
	function_name string // function name if external
	arguments     []Expr
	position      Position
}

// MatchExpr represents pattern matching
pub struct MatchExpr {
pub:
	value    Expr
	cases    []MatchCase
	position Position
}

// ListConsExpr represents list cons operation
pub struct ListConsExpr {
pub:
	head     Expr
	tail     Expr
	position Position
}

// ListEmptyExpr represents an empty list
pub struct ListEmptyExpr {
	position Position
}

// ListLiteralExpr represents a list literal
pub struct ListLiteralExpr {
pub:
	elements []Expr
	position Position
}

// TupleExpr represents a tuple
pub struct TupleExpr {
pub:
	elements []Expr
	position Position
}

// MapLiteralExpr represents a map literal
pub struct MapLiteralExpr {
pub:
	entries  []MapEntry
	position Position
}

// MapAccessExpr represents map access expression
pub struct MapAccessExpr {
pub:
	map_expr Expr
	key      Expr
	position Position
}

// MapUpdateExpr represents map update operations
pub struct MapUpdateExpr {
pub:
	base_map Expr
	entries  []MapEntry
	position Position
}

// RecordLiteralExpr represents a record literal
pub struct RecordLiteralExpr {
pub:
	name     string
	fields   []RecordField
	position Position
}

// RecordAccessExpr represents record field access
pub struct RecordAccessExpr {
pub:
	record   Expr
	field    string
	position Position
}

// RecordUpdateExpr represents record update operations
pub struct RecordUpdateExpr {
pub:
	record_name string
	base_record Expr
	fields      []RecordField
	position    Position
}

// FunExpr represents a function definition
pub struct FunExpr {
pub:
	parameters []Pattern
	body       BlockExpr
	position   Position
}

// SendExpr represents message sending
pub struct SendExpr {
pub:
	pid      Expr
	message  Expr
	position Position
}

// ReceiveExpr represents message receiving
pub struct ReceiveExpr {
pub:
	cases    []ReceiveCase
	timeout  ?TimeoutClause
	position Position
}

// TimeoutClause represents a timeout clause in receive expressions
pub struct TimeoutClause {
pub:
	timeout  Expr      // Timeout value in milliseconds
	body     BlockExpr // Code to execute on timeout
	position Position
}

// GuardExpr represents a guard expression
pub struct GuardExpr {
pub:
	condition Expr
	position  Position
}

// UnaryExpr represents a unary operation
pub struct UnaryExpr {
pub:
	op       UnaryOp
	operand  Expr
	position Position
}

// IfExpr represents an if expression
pub struct IfExpr {
pub:
	condition Expr
	then_body BlockExpr
	else_body BlockExpr
	position  Position
}

// CaseExpr represents a case expression
pub struct CaseExpr {
pub:
	value    Expr
	cases    []MatchCase
	position Position
}

// WithExpr represents a with expression
pub struct WithExpr {
pub:
	bindings  []WithBinding
	body      BlockExpr
	else_body BlockExpr
	position  Position
}

// ForExpr represents a for expression (list comprehension)
pub struct ForExpr {
pub:
	pattern    Pattern
	collection Expr
	guard      Expr
	body       BlockExpr
	position   Position
}

// SimpleMatchExpr represents a simple match expression (without rescue)
pub struct SimpleMatchExpr {
pub:
	pattern  Pattern
	value    Expr
	guard    Expr
	position Position
}

// MatchRescueExpr represents a match rescue expression
pub struct MatchRescueExpr {
pub:
	pattern     Pattern
	value       Expr
	rescue_var  string
	rescue_body BlockExpr
	position    Position
}

// BlockExpr represents a block expression (do...end)
pub struct BlockExpr {
pub:
	body     []Stmt
	position Position
}

// Expr represents expressions in LX using sum types
pub type Expr = VariableExpr
	| LiteralExpr
	| AssignExpr
	| BinaryExpr
	| CallExpr
	| MatchExpr
	| ListConsExpr
	| ListEmptyExpr
	| ListLiteralExpr
	| TupleExpr
	| MapLiteralExpr
	| MapAccessExpr
	| MapUpdateExpr
	| RecordLiteralExpr
	| RecordAccessExpr
	| RecordUpdateExpr
	| FunExpr
	| SendExpr
	| ReceiveExpr
	| GuardExpr
	| UnaryExpr
	| IfExpr
	| CaseExpr
	| WithExpr
	| ForExpr
	| SimpleMatchExpr
	| MatchRescueExpr
	| BlockExpr

// BinaryOp represents binary operators
pub enum BinaryOp {
	add
	subtract
	multiply
	divide
	modulo
	power
	equal
	not_equal
	less_than
	less_equal
	greater_than
	greater_equal
	and
	or
	cons
	append
}

// str returns a string representation of BinaryOp
pub fn (op BinaryOp) str() string {
	return match op {
		.add { '+' }
		.subtract { '-' }
		.multiply { '*' }
		.divide { '/' }
		.modulo { '%' }
		.power { '**' }
		.equal { '==' }
		.not_equal { '!=' }
		.less_than { '<' }
		.less_equal { '<=' }
		.greater_than { '>' }
		.greater_equal { '>=' }
		.and { 'and' }
		.or { 'or' }
		.cons { '::' }
		.append { '++' }
	}
}

// UnaryOp represents unary operators
pub enum UnaryOp {
	not
	minus
}

// str returns a string representation of UnaryOp
pub fn (op UnaryOp) str() string {
	return match op {
		.not { 'not' }
		.minus { '-' }
	}
}

// WildcardPattern represents a wildcard pattern
pub struct WildcardPattern {
	position Position
}

// VarPattern represents a variable pattern
pub struct VarPattern {
pub:
	name            string
	position        Position
	type_annotation ?TypeExpression
}

// LiteralPattern represents a literal pattern
pub struct LiteralPattern {
pub:
	value Literal
}

// AtomPattern represents an atom pattern
pub struct AtomPattern {
pub:
	value    string
	position Position
}

// ListConsPattern represents list cons pattern
pub struct ListConsPattern {
pub:
	head Pattern
	tail Pattern
}

// ListEmptyPattern represents an empty list pattern
pub struct ListEmptyPattern {
	position Position
}

// ListLiteralPattern represents a list literal pattern
pub struct ListLiteralPattern {
pub:
	elements []Pattern
}

// TuplePattern represents a tuple pattern
pub struct TuplePattern {
pub:
	elements []Pattern
}

// MapPattern represents a map pattern
pub struct MapPattern {
pub:
	entries []MapPatternEntry
}

// RecordPattern represents a record pattern
pub struct RecordPattern {
pub:
	name   string
	fields []RecordPatternField
}

// BinaryPattern represents a binary pattern
pub struct BinaryPattern {
pub:
	segments []BinarySegment
}

// Pattern represents patterns in LX using sum types
pub type Pattern = WildcardPattern
	| VarPattern
	| LiteralPattern
	| AtomPattern
	| ListConsPattern
	| ListEmptyPattern
	| ListLiteralPattern
	| TuplePattern
	| MapPattern
	| RecordPattern
	| BinaryPattern

// str returns a string representation of Pattern
pub fn (p Pattern) str() string {
	return match p {
		WildcardPattern {
			'PWildcard'
		}
		VarPattern {
			if type_ann := p.type_annotation {
				'PVar(${p.name} :: ${type_ann.str()})'
			} else {
				'PVar(${p.name})'
			}
		}
		LiteralPattern {
			'PLiteral(${p.value.str()})'
		}
		AtomPattern {
			'PAtom(${p.value})'
		}
		ListConsPattern {
			'PCons(${p.head.str()}, ${p.tail.str()})'
		}
		ListEmptyPattern {
			'PEmpty'
		}
		ListLiteralPattern {
			'PList([${p.elements.map(it.str()).join(', ')}])'
		}
		TuplePattern {
			'PTuple([${p.elements.map(it.str()).join(', ')}])'
		}
		MapPattern {
			'PMap([${p.entries.map(it.str()).join(', ')}])'
		}
		RecordPattern {
			'PRecord(${p.name}, [${p.fields.map(it.str()).join(', ')}])'
		}
		BinaryPattern {
			'PBinary([${p.segments.map(it.str()).join(', ')}])'
		}
	}
}

// ExprStmt represents an expression statement
pub struct ExprStmt {
pub:
	expr Expr
}

// ModuleStmt represents a module statement
pub struct ModuleStmt {
pub mut:
	name       string
	exports    []string
	imports    []Import
	statements []Stmt
	position   Position
}

// FunctionStmt represents a function statement
pub struct FunctionStmt {
pub:
	id         string // Unique function identifier
	name       string
	clauses    []FunctionClause
	is_private bool
	directives []string
	position   Position
}

// RecordDefStmt represents a record definition
pub struct RecordDefStmt {
pub:
	name     string
	fields   []RecordFieldDef
	position Position
}

// TypeDefStmt represents a type definition
pub struct TypeDefStmt {
pub:
	name       string
	definition TypeDef
	position   Position
}

// TypeAliasStmt represents a type alias definition (type name :: type_expression)
pub struct TypeAliasStmt {
pub:
	name       string
	type_expr  TypeExpression
	alias_type TypeAliasType // opaque, nominal, or regular
	position   Position
}

// TypeAliasType represents the type of type alias
pub enum TypeAliasType {
	regular // regular type alias
	opaque  // opaque type alias
	nominal // nominal type alias
}

// Stmt represents statements in LX using sum types
pub type Stmt = ExprStmt
	| ModuleStmt
	| FunctionStmt
	| RecordDefStmt
	| TypeDefStmt
	| TypeAliasStmt

// str returns a string representation of Expr
pub fn (e Expr) str() string {
	return match e {
		VariableExpr {
			'Var(${e.name})'
		}
		LiteralExpr {
			'Literal(${e.value.str()})'
		}
		AssignExpr {
			if type_ann := e.type_annotation {
				'Assign(${e.name} :: ${type_ann.str()}, ${e.value.str()})'
			} else {
				'Assign(${e.name}, ${e.value.str()})'
			}
		}
		BinaryExpr {
			'Binary(${e.left.str()} ${e.op.str()} ${e.right.str()})'
		}
		CallExpr {
			if e.external {
				'ExternalCall(${e.module}:${e.function_name}, [${e.arguments.map(it.str()).join(', ')}])'
			} else {
				'Call(${e.function.str()}, [${e.arguments.map(it.str()).join(', ')}])'
			}
		}
		MatchExpr {
			'Match(${e.value.str()}, [${e.cases.map(it.str()).join(', ')}])'
		}
		ListConsExpr {
			'Cons(${e.head.str()}, ${e.tail.str()})'
		}
		ListEmptyExpr {
			'Empty'
		}
		ListLiteralExpr {
			'List([${e.elements.map(it.str()).join(', ')}])'
		}
		TupleExpr {
			'Tuple([${e.elements.map(it.str()).join(', ')}])'
		}
		MapLiteralExpr {
			'Map([${e.entries.map(it.str()).join(', ')}])'
		}
		RecordLiteralExpr {
			'Record(${e.name}, [${e.fields.map(it.str()).join(', ')}])'
		}
		RecordAccessExpr {
			'Access(${e.record.str()}.${e.field})'
		}
		RecordUpdateExpr {
			'RecordUpdate(${e.record_name}, ${e.base_record.str()}, [${e.fields.map(it.str()).join(', ')}])'
		}
		FunExpr {
			'Fun([${e.parameters.map(it.str()).join(', ')}], ${e.body.str()})'
		}
		SendExpr {
			'Send(${e.pid.str()}, ${e.message.str()})'
		}
		ReceiveExpr {
			timeout_str := if timeout_clause := e.timeout {
				timeout_clause.str()
			} else {
				'none'
			}
			'Receive([${e.cases.map(it.str()).join(', ')}], ${timeout_str})'
		}
		GuardExpr {
			'Guard(${e.condition.str()})'
		}
		UnaryExpr {
			'Unary(${e.op.str()}, ${e.operand.str()})'
		}
		MapAccessExpr {
			'Access(${e.map_expr.str()}[${e.key.str()}])'
		}
		MapUpdateExpr {
			'MapUpdate(${e.base_map.str()}, [${e.entries.map(it.str()).join(', ')}])'
		}
		IfExpr {
			'If(${e.condition.str()}, ${e.then_body.str()}, ${e.else_body.str()})'
		}
		CaseExpr {
			'Case(${e.value.str()}, [${e.cases.map(it.str()).join(', ')}])'
		}
		WithExpr {
			'With([${e.bindings.map(it.str()).join(', ')}], ${e.body.str()}, ${e.else_body.str()})'
		}
		ForExpr {
			'For(${e.pattern.str()}, ${e.collection.str()}, ${e.guard.str()}, ${e.body.str()})'
		}
		SimpleMatchExpr {
			'SimpleMatch(${e.pattern.str()} <- ${e.value.str()})'
		}
		MatchRescueExpr {
			'MatchRescue(${e.pattern.str()} <- ${e.value.str()} rescue ${e.rescue_var} ${e.rescue_body.str()})'
		}
		BlockExpr {
			'Block([${e.body.map(it.str()).join(', ')}])'
		}
	}
}

// MatchCase represents a match case
pub struct MatchCase {
pub:
	pattern  Pattern
	guard    Expr
	body     BlockExpr
	position Position
}

// ReceiveCase represents a receive case
pub struct ReceiveCase {
pub:
	pattern  Pattern
	guard    Expr
	body     BlockExpr
	position Position
}

// MapEntry represents a map entry
pub struct MapEntry {
pub:
	key      Expr
	value    Expr
	position Position
}

// str returns a string representation of MapEntry
pub fn (me MapEntry) str() string {
	return '${me.key.str()} => ${me.value.str()}'
}

// MapPatternEntry represents a map pattern entry
pub struct MapPatternEntry {
pub:
	key      Pattern
	value    Pattern
	position Position
}

// RecordField represents a record field
pub struct RecordField {
pub:
	name     string
	value    Expr
	position Position
}

// RecordPatternField represents a record pattern field
pub struct RecordPatternField {
pub:
	name     string
	pattern  Pattern
	position Position
}

// RecordFieldDef represents a record field definition
pub struct RecordFieldDef {
pub:
	name       string
	field_type LXType
	position   Position
}

// FunctionClause represents a function clause
pub struct FunctionClause {
pub:
	parameters []Pattern
	guard      Expr
	body       BlockExpr
	position   Position
}

// Import represents an import statement
pub struct Import {
pub:
	module   string
	aliases  []string
	position Position
}

// BinarySegment represents a binary segment
pub struct BinarySegment {
pub:
	size     int
	unit     string
	position Position
}

// TypeDef represents a type definition
pub type TypeDef = SimpleTypeDef | UnionTypeDef | RecordTypeDef | FunctionTypeDef

// SimpleTypeDef represents a simple type definition
pub struct SimpleTypeDef {
pub:
	base_type LXType
}

// UnionTypeDef represents a union type definition
pub struct UnionTypeDef {
pub:
	types []LXType
}

// RecordTypeDef represents a record type definition
pub struct RecordTypeDef {
pub:
	fields []RecordFieldDef
}

// FunctionTypeDef represents a function type definition
pub struct FunctionTypeDef {
pub:
	parameters  []LXType
	return_type LXType
}

// WithBinding represents a binding in a with expression
pub struct WithBinding {
pub:
	pattern  Pattern
	value    Expr
	guard    Expr
	position Position
}

// TypeExpression represents type expressions in type annotations and definitions
pub type TypeExpression = SimpleTypeExpr
	| UnionTypeExpr
	| ListTypeExpr
	| TupleTypeExpr
	| MapTypeExpr
	| FunctionTypeExpr
	| VariableTypeExpr

// SimpleTypeExpr represents a simple type like integer, string, atom
pub struct SimpleTypeExpr {
pub:
	name     string
	position Position
}

// UnionTypeExpr represents union types like integer | float
pub struct UnionTypeExpr {
pub:
	types    []TypeExpression
	position Position
}

// ListTypeExpr represents list types like list(integer)
pub struct ListTypeExpr {
pub:
	element_type TypeExpression
	position     Position
}

// TupleTypeExpr represents tuple types like {integer, string}
pub struct TupleTypeExpr {
pub:
	element_types []TypeExpression
	position      Position
}

// MapTypeExpr represents map types like map(atom, string)
pub struct MapTypeExpr {
pub:
	key_type   TypeExpression
	value_type TypeExpression
	position   Position
}

// FunctionTypeExpr represents function types like (integer, string) -> boolean
pub struct FunctionTypeExpr {
pub:
	param_types []TypeExpression
	return_type TypeExpression
	position    Position
}

// VariableTypeExpr represents type variables like 'a' or 'T'
pub struct VariableTypeExpr {
pub:
	name     string
	position Position
}

// str returns a string representation of TypeExpression
pub fn (te TypeExpression) str() string {
	return match te {
		SimpleTypeExpr {
			te.name
		}
		UnionTypeExpr {
			te.types.map(it.str()).join(' | ')
		}
		ListTypeExpr {
			'list(${te.element_type.str()})'
		}
		TupleTypeExpr {
			'{${te.element_types.map(it.str()).join(', ')}}'
		}
		MapTypeExpr {
			'map(${te.key_type.str()}, ${te.value_type.str()})'
		}
		FunctionTypeExpr {
			if te.param_types.len == 0 {
				'() -> ${te.return_type.str()}'
			} else {
				'(${te.param_types.map(it.str()).join(', ')}) -> ${te.return_type.str()}'
			}
		}
		VariableTypeExpr {
			te.name
		}
	}
}

// StringLiteral represents a string literal
pub struct StringLiteral {
pub:
	value    string
	position Position
}

// IntegerLiteral represents an integer literal
pub struct IntegerLiteral {
pub:
	value    int
	position Position
}

// FloatLiteral represents a float literal
pub struct FloatLiteral {
pub:
	value    f64
	position Position
}

// BooleanLiteral represents a boolean literal
pub struct BooleanLiteral {
pub:
	value    bool
	position Position
}

// AtomLiteral represents an atom literal
pub struct AtomLiteral {
pub:
	value    string
	position Position
}

// NilLiteral represents a nil literal
pub struct NilLiteral {
pub:
	position Position
}

// str returns a string representation of ReceiveCase
pub fn (rc ReceiveCase) str() string {
	return 'ReceiveCase(${rc.pattern.str()} when ${rc.guard.str()} -> ${rc.body.str()})'
}

// str returns a string representation of TimeoutClause
pub fn (tc TimeoutClause) str() string {
	return 'TimeoutClause(${tc.timeout.str()} -> ${tc.body.str()})'
}
