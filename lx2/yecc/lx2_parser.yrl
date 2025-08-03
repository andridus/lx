Nonterminals
program function_def block statement expression literal variable_binding.

Terminals
def do end integer float string atom boolean nil identifier underscore '(' ')' equals semicolon.

Rootsymbol program.

program -> function_def : ['$1'].
program -> function_def program : ['$1' | '$2'].

function_def -> def identifier '(' ')' do block end :
    {function_def, extract_identifier('$2'), [], '$6'}.

block -> statement : ['$1'].
block -> statement block : ['$1' | '$2'].
block -> statement semicolon statement : ['$1', '$3'].
block -> statement semicolon block : ['$1' | '$3'].

statement -> variable_binding : '$1'.
statement -> expression : '$1'.

expression -> literal : '$1'.
expression -> identifier : {variable_ref, extract_identifier('$1')}.

variable_binding -> identifier equals expression :
    {variable_binding, extract_identifier('$1'), '$3'}.

literal -> integer : {literal, integer, extract_integer('$1')}.
literal -> float : {literal, float, extract_float('$1')}.
literal -> string : {literal, string, extract_string('$1')}.
literal -> atom : {literal, atom, extract_atom('$1')}.
literal -> boolean : {literal, boolean, extract_boolean('$1')}.
literal -> nil : {literal, nil, extract_nil('$1')}.
literal -> underscore : {literal, undefined, extract_underscore('$1')}.

Erlang code.

extract_identifier({identifier, _, Value}) when is_list(Value) -> list_to_atom(Value);
extract_identifier({identifier, _, Value}) -> Value.
extract_integer({integer, _, Value}) -> Value.
extract_float({float, _, Value}) -> Value.
extract_string({string, _, Value}) -> Value.
extract_atom({atom, _, Value}) -> Value.
extract_boolean({boolean, _, Value}) -> Value.
extract_nil({nil, _, Value}) -> Value.
extract_underscore({underscore, _}) -> undefined.