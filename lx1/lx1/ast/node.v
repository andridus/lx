module ast

// Estrutura unificada da AST
pub struct Node {
pub:
    id       int         // ID único para type table
    kind     NodeKind    // Tipo do nó
    value    string      // Valor quando aplicável (sempre string para simplificar)
    children []Node      // Filhos do nó
    position Position    // Posição no código
    type_    Type        // Tipo inferido
}

pub enum NodeKind {
    // Literals
    integer
    float
    string
    boolean
    atom
    nil

    // Function structure
    function      // Definição de função
    function_body // Corpo da função

    // Module structure
    module        // Raiz do módulo
}

pub struct Position {
pub:
    line   int
    column int
    file   string
}

pub struct Type {
pub:
    name   string
    params []Type
}

// Helper methods for Node
pub fn (n Node) str() string {
    return match n.kind {
        .integer { 'Int(${n.value})' }
        .float { 'Float(${n.value})' }
        .string { 'String("${n.value}")' }
        .boolean { 'Bool(${n.value})' }
        .atom { 'Atom(${n.value})' }
        .nil { 'Nil' }
        .function { 'Function(${n.value})' }
        .function_body { 'FunctionBody' }
        .module { 'Module' }
    }
}

pub fn (p Position) str() string {
    return '${p.file}:${p.line}:${p.column}'
}

pub fn (t Type) str() string {
    if t.params.len == 0 {
        return t.name
    }
    params_str := t.params.map(it.str()).join(', ')
    return '${t.name}(${params_str})'
}