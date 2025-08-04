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
  map_entry.

Terminals
  '{' '}' '[' ']' '(' ')' ',' ':' '%'
  integer float atom string.

Rootsymbol program.

program -> expression : ['$1'].
program -> expression program : ['$1' | '$2'].

expression -> literal : '$1'.
expression -> tuple : '$1'.
expression -> list : '$1'.
expression -> map : '$1'.

literal -> atom : {atom, line('$1'), element(3, '$1')}.
literal -> integer : {integer, line('$1'), element(3, '$1')}.
literal -> float : {float, line('$1'), element(3, '$1')}.
literal -> string : {string, line('$1'), element(3, '$1')}.

tuple -> '{' '}' : {tuple, line('$1'), []}.
tuple -> '{' expression '}' : {tuple, line('$1'), ['$2']}.
tuple -> '{' expression tuple_rest '}' : {tuple, line('$1'), ['$2' | '$3']}.

tuple_rest -> ',' expression : ['$2'].
tuple_rest -> ',' expression tuple_rest : ['$2' | '$3'].

list -> '[' ']' : {list, line('$1'), []}.
list -> '[' expression ']' : {list, line('$1'), ['$2']}.
list -> '[' expression list_rest ']' : {list, line('$1'), ['$2' | '$3']}.

list_rest -> ',' expression : ['$2'].
list_rest -> ',' expression list_rest : ['$2' | '$3'].

map -> '%' '{' '}' : {map, line('$2'), []}.
map -> '%' '{' map_entries '}' : {map, line('$2'), '$3'}.

map_entries -> map_entry : ['$1'].
map_entries -> map_entry ',' map_entries : ['$1' | '$3'].

map_entry -> expression ':' expression : {map_entry, line('$1'), '$1', '$3'}.

Erlang code.

line({_, Line, _}) -> Line;
line({_, Line}) -> Line;
line(Line) when is_integer(Line) -> Line.