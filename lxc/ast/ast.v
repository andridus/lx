module ast

// VariableExpr represents a variable reference
pub struct VariableExpr {
pub:
	name string
}

// LiteralExpr represents a literal expression
pub struct LiteralExpr {
pub:
	value Literal
}

// AssignExpr represents an assignment expression
pub struct AssignExpr {
pub:
	name     string
	value    Expr
	position Position
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
	function  Expr
	arguments []Expr
	position  Position
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

// FunExpr represents a function definition
pub struct FunExpr {
pub:
	parameters []Pattern
	body       []Stmt
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
	timeout  Expr
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

// MapAccessExpr represents map access expression
pub struct MapAccessExpr {
pub:
	map_expr Expr
	key      Expr
	position Position
}

// IfExpr represents an if expression
pub struct IfExpr {
pub:
	condition Expr
	then_body []Stmt
	else_body []Stmt
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
	body      []Stmt
	else_body []Stmt
	position  Position
}

// ForExpr represents a for expression (list comprehension)
pub struct ForExpr {
pub:
	pattern    Pattern
	collection Expr
	guard      Expr
	body       []Stmt
	position   Position
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
	| RecordLiteralExpr
	| RecordAccessExpr
	| FunExpr
	| SendExpr
	| ReceiveExpr
	| GuardExpr
	| UnaryExpr
	| MapAccessExpr
	| IfExpr
	| CaseExpr
	| WithExpr
	| ForExpr

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
	andalso
	orelse
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
		.and { '&&' }
		.or { '||' }
		.andalso { 'andalso' }
		.orelse { 'orelse' }
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
}

// VarPattern represents a variable pattern
pub struct VarPattern {
pub:
	name string
}

// LiteralPattern represents a literal pattern
pub struct LiteralPattern {
pub:
	value Literal
}

// AtomPattern represents an atom pattern
pub struct AtomPattern {
pub:
	value string
}

// ListConsPattern represents list cons pattern
pub struct ListConsPattern {
pub:
	head Pattern
	tail Pattern
}

// ListEmptyPattern represents an empty list pattern
pub struct ListEmptyPattern {
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
		WildcardPattern { 'PWildcard' }
		VarPattern { 'PVar(${p.name})' }
		LiteralPattern { 'PLiteral(${p.value.str()})' }
		AtomPattern { 'PAtom(${p.value})' }
		ListConsPattern { 'PCons(${p.head.str()}, ${p.tail.str()})' }
		ListEmptyPattern { 'PEmpty' }
		ListLiteralPattern { 'PList([${p.elements.map(it.str()).join(', ')}])' }
		TuplePattern { 'PTuple([${p.elements.map(it.str()).join(', ')}])' }
		MapPattern { 'PMap([${p.entries.map(it.str()).join(', ')}])' }
		RecordPattern { 'PRecord(${p.name}, [${p.fields.map(it.str()).join(', ')}])' }
		BinaryPattern { 'PBinary([${p.segments.map(it.str()).join(', ')}])' }
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
	name       string
	clauses    []FunctionClause
	is_private bool
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

// Stmt represents statements in LX using sum types
pub type Stmt = ExprStmt | ModuleStmt | FunctionStmt | RecordDefStmt | TypeDefStmt

// str returns a string representation of Expr
pub fn (e Expr) str() string {
	return match e {
		VariableExpr { 'Var(${e.name})' }
		LiteralExpr { 'Literal(${e.value.str()})' }
		AssignExpr { 'Assign(${e.name}, ${e.value.str()})' }
		BinaryExpr { 'Binary(${e.left.str()} ${e.op.str()} ${e.right.str()})' }
		CallExpr { 'Call(${e.function.str()}, [${e.arguments.map(it.str()).join(', ')}])' }
		MatchExpr { 'Match(${e.value.str()}, [${e.cases.map(it.str()).join(', ')}])' }
		ListConsExpr { 'Cons(${e.head.str()}, ${e.tail.str()})' }
		ListEmptyExpr { 'Empty' }
		ListLiteralExpr { 'List([${e.elements.map(it.str()).join(', ')}])' }
		TupleExpr { 'Tuple([${e.elements.map(it.str()).join(', ')}])' }
		MapLiteralExpr { 'Map([${e.entries.map(it.str()).join(', ')}])' }
		RecordLiteralExpr { 'Record(${e.name}, [${e.fields.map(it.str()).join(', ')}])' }
		RecordAccessExpr { 'Access(${e.record.str()}.${e.field})' }
		FunExpr { 'Fun([${e.parameters.map(it.str()).join(', ')}], [${e.body.map(it.str()).join(', ')}])' }
		SendExpr { 'Send(${e.pid.str()}, ${e.message.str()})' }
		ReceiveExpr { 'Receive([${e.cases.map(it.str()).join(', ')}], ${e.timeout.str()})' }
		GuardExpr { 'Guard(${e.condition.str()})' }
		UnaryExpr { 'Unary(${e.op.str()}, ${e.operand.str()})' }
		MapAccessExpr { 'Access(${e.map_expr.str()}[${e.key.str()}])' }
		IfExpr { 'If(${e.condition.str()}, [${e.then_body.map(it.str()).join(', ')}], [${e.else_body.map(it.str()).join(', ')}])' }
		CaseExpr { 'Case(${e.value.str()}, [${e.cases.map(it.str()).join(', ')}])' }
		WithExpr { 'With([${e.bindings.map(it.str()).join(', ')}], [${e.body.map(it.str()).join(', ')}], [${e.else_body.map(it.str()).join(', ')}])' }
		ForExpr { 'For(${e.pattern.str()}, ${e.collection.str()}, ${e.guard.str()}, [${e.body.map(it.str()).join(', ')}])' }
	}
}

// MatchCase represents a match case
pub struct MatchCase {
pub:
	pattern  Pattern
	guard    Expr
	body     []Stmt
	position Position
}

// ReceiveCase represents a receive case
pub struct ReceiveCase {
pub:
	pattern  Pattern
	guard    Expr
	body     []Stmt
	position Position
}

// MapEntry represents a map entry
pub struct MapEntry {
pub:
	key      Expr
	value    Expr
	position Position
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
	body       []Stmt
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
	position Position
}
