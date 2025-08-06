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
  macro_definition
  macro_body
  macro_parameters
  macro_parameter
  do_block
  expression_list.

Terminals
  '{' '}' '[' ']' '(' ')' ',' ':' '%' ';'
  defmacro do end_ infix
  operator
  integer float atom string.

Rootsymbol program.

program -> expression : ['$1'].
program -> expression program : ['$1' | '$2'].
program -> macro_definition : ['$1'].
program -> macro_definition program : ['$1' | '$2'].

expression -> literal : '$1'.
expression -> tuple : '$1'.
expression -> list : '$1'.
expression -> map : '$1'.
expression -> atom do_block : {macro_call, line('$1'), element(3, '$1'), '$2'}.
expression -> atom list : {macro_call, line('$1'), element(3, '$1'), '$2'}.
expression -> expression operator expression : {possible_macro_call, line('$2'), element(3, '$2'), ['$1', '$3']}.

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

% Macro definitions
macro_definition -> defmacro atom '(' ')' do macro_body end_ :
    {macro_def, line('$1'), element(3, '$2'), [], '$6'}.

macro_definition -> defmacro atom '(' macro_parameters ')' do macro_body end_ :
    {macro_def, line('$1'), element(3, '$2'), '$4', '$7'}.

macro_definition -> defmacro infix operator '(' macro_parameters ')' do macro_body end_ :
    {macro_def_infix, line('$1'), element(3, '$3'), '$5', '$8'}.

macro_definition -> defmacro infix atom '(' macro_parameters ')' do macro_body end_ :
    {macro_def_infix, line('$1'), element(3, '$3'), '$5', '$8'}.

macro_parameters -> macro_parameter : ['$1'].
macro_parameters -> macro_parameter ',' macro_parameters : ['$1' | '$3'].

macro_parameter -> atom : '$1'.

macro_body -> expression : '$1'.
macro_body -> do_block : '$1'.

% Do/end blocks
do_block -> do expression_list end_ : {do_block, line('$1'), '$2'}.
do_block -> do end_ : {do_block, line('$1'), []}.

expression_list -> expression : ['$1'].
expression_list -> expression ';' expression_list : ['$1' | '$3'].
expression_list -> expression expression_list : ['$1' | '$2'].

Erlang code.

line({_, Line, _}) -> Line;
line({_, Line}) -> Line;
line(Line) when is_integer(Line) -> Line.