Nonterminals
    expr program statements.

Terminals
    integer atom ident semicolon.

Rootsymbol program.

program -> statements : '$1'.

statements -> expr : ['$1'].
statements -> expr semicolon statements : ['$1' | '$3'].

expr -> integer : '$1'.
expr -> atom : '$1'.
expr -> ident : '$1'.