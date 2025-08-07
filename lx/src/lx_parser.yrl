Nonterminals
  program
  expression
  literal
  tuple
  tuple_rest
  list
  list_rest
  map
  map_entries
  map_entry
  with_expr
  clause
  clause_rest
  block
  local_call
  arg_rest
  expression_list.

Terminals
  '{' '}' '[' ']' '(' ')' ',' ':' '%' ';'
  do end_
  with match
  operator
  arrow_l dbl_arrow
  integer float atom string ident mod_fun key .

Rootsymbol program.

program -> expression : ['$1'].
program -> expression program : ['$1' | '$2'].

expression -> literal : '$1'.
expression -> tuple : '$1'.
expression -> list : '$1'.
expression -> map : '$1'.
expression -> ident : '$1'.
expression -> local_call : '$1'.

expression -> block : '$1'.
expression -> with with_expr block: {with, line('$1'), ['$2', '$3']}.
expression -> match clause: {match, line('$1'), ['$2']}.
expression -> mod_fun '(' ')': {mfa, line('$1'), [element(1, element(3, '$1')), element(2, element(3, '$1')), []]}.
expression -> mod_fun '(' expression ')': {mfa, line('$1'), [element(1, element(3, '$1')), element(2, element(3, '$1')), ['$3']]}.
expression -> mod_fun '(' expression arg_rest ')': {mfa, line('$1'), [element(1, element(3, '$1')), element(2, element(3, '$1')),  ['$3' | '$4']]}.

expression -> key expression : {tuple, line('$1'), [{atom, line('$1'), element(3, '$1')}, '$2']}.
expression -> ident expression : {element(3, '$1'), line('$1'), ['$2']}.
% expression -> ident expression expression_list : {element(3, '$1'), line('$1'), ['$2' | '$3']}.
expression -> expression operator expression : {element(3, '$2'), line('$2'), ['$1', '$3']}.
expression -> expression dbl_arrow expression : {element(3, '$2'), line('$2'), ['$1', '$3']}.

with_expr -> clause : '$1'.
with_expr -> clause clause_rest : ['$1' | '$2'].

clause -> expression arrow_l expression: {clause, line('$1'), ['$1', '$3']}.
clause_rest -> ',' clause : ['$2'].
clause_rest -> ',' clause clause_rest : ['$2' | '$3'].

local_call -> ident '(' ')': {mfa, line('$1'), ['__MODULE__', element(3, '$1'), []]}.
local_call -> ident '(' expression ')': {mfa, line('$1'), ['__MODULE__', element(3, '$1'), ['$3']]}.
local_call -> ident '(' expression arg_rest ')': {mfa, line('$1'), ['__MODULE__', element(3, '$1'),  ['$3' | '$4']]}.

literal -> atom : {atom, line('$1'), element(3, '$1')}.
literal -> integer : {integer, line('$1'), element(3, '$1')}.
literal -> float : {float, line('$1'), element(3, '$1')}.
literal -> string : {string, line('$1'), element(3, '$1')}.

tuple -> '{' '}' : {tuple, line('$1'), []}.
tuple -> '{' expression '}' : {tuple, line('$1'), ['$2']}.
tuple -> '{' expression tuple_rest '}' : {tuple, line('$1'), ['$2' | '$3']}.

tuple_rest -> ',' expression : ['$2'].
tuple_rest -> ',' expression tuple_rest : ['$2' | '$3'].

arg_rest -> ',' expression : ['$2'].
arg_rest -> ',' expression arg_rest : ['$2' | '$3'].

list -> '[' ']' : {list, line('$1'), []}.
list -> '[' expression ']' : {list, line('$1'), ['$2']}.
list -> '[' expression list_rest ']' : {list, line('$1'), ['$2' | '$3']}.

list_rest -> ',' expression : ['$2'].
list_rest -> ',' expression list_rest : ['$2' | '$3'].

map -> '%' '{' '}' : {map, line('$2'), []}.
map -> '%' '{' map_entries '}' : {map, line('$2'), '$3'}.

map_entries -> map_entry : ['$1'].
map_entries -> map_entry ',' map_entries : ['$1' | '$3'].

map_entry -> key expression : {map_entry, line('$1'), [{atom, line('$1'), element(3, '$1')}, '$2']}.
map_entry -> expression ':' expression : {map_entry, line('$1'), '$1', '$3'}.


% Do/end blocks
block -> do expression_list end_ : {block, line('$1'), '$2'}.
block -> do end_ : {block, line('$1'), []}.

expression_list -> expression : ['$1'].
expression_list -> expression ';' expression_list : ['$1' | '$3'].
expression_list -> expression expression_list : ['$1' | '$2'].

Erlang code.

line({_, Line, _}) -> Line;
line({_, Line}) -> Line;
line(Line) when is_integer(Line) -> Line.