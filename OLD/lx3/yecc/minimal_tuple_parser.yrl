Nonterminals
    expr tuple tuple_elements.

Terminals
    integer ',' '{' '}'.

Rootsymbol expr.

expr -> tuple : '$1'.
expr -> integer : '$1'.

tuple -> '{' tuple_elements '}' : {tuple, '$2'}.

tuple_elements -> integer : ['$1'].
tuple_elements -> integer ',' tuple_elements : ['$1' | '$3'].