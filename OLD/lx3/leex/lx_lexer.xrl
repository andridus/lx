Definitions.

D = [0-9]
L = [a-zA-Z_]
WS = [\s\t]
NL = \n|\r\n|\r

Rules.

% Literals básicos do Erlang
:'[^']+' : {token, {atom, TokenLine, TokenCol, list_to_atom(lists:sublist(TokenChars, 3, length(TokenChars)-3))}}.
{D}+ : {token, {integer, TokenLine, TokenCol, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, TokenCol, list_to_float(TokenChars)}}.
"[^"]*" : {token, {string, TokenLine, TokenCol, strip_quotes(TokenChars)}}.
:({L}({L}|{D})*) : {token, {atom, TokenLine, TokenCol, strip_colon(TokenChars)}}.
true : {token, {boolean, TokenLine, TokenCol, true}}.
false : {token, {boolean, TokenLine, TokenCol, false}}.
nil : {token, {nil, TokenLine, TokenCol, nil}}.

% Keywords básicas (apenas primitivas)
defmacro : {token, {defmacro, TokenLine, TokenCol}}.
infix : {token, {infix, TokenLine, TokenCol}}.
do : {token, {do, TokenLine, TokenCol}}.
end : {token, {'end', TokenLine, TokenCol}}.
'->' : {token, {'->', TokenLine, TokenCol}}.

% Type annotation operator
:: : {token, {'::', TokenLine, TokenCol}}.

% Type identifiers (start with uppercase, but not keywords)
{A-Z}({L}|{D})* :
    case TokenChars of
        "Integer" -> {token, {type_identifier, TokenLine, TokenCol, "integer"}};
        "Float" -> {token, {type_identifier, TokenLine, TokenCol, "float"}};
        "String" -> {token, {type_identifier, TokenLine, TokenCol, "string"}};
        "Boolean" -> {token, {type_identifier, TokenLine, TokenCol, "boolean"}};
        "Atom" -> {token, {type_identifier, TokenLine, TokenCol, "atom"}};
        "Any" -> {token, {type_identifier, TokenLine, TokenCol, "any"}};
        "Fun" -> {token, {type_identifier, TokenLine, TokenCol, "fun"}};
        "List" -> {token, {type_identifier, TokenLine, TokenCol, "list"}};
        "Tuple" -> {token, {type_identifier, TokenLine, TokenCol, "tuple"}};
        "Number" -> {token, {type_identifier, TokenLine, TokenCol, "number"}};
        "number" -> {token, {type_identifier, TokenLine, TokenCol, "number"}};
        _ -> {token, {ident, TokenLine, TokenCol, TokenChars}}
    end.

% Type keywords
integer : {token, {type_identifier, TokenLine, TokenCol, "integer"}}.
float : {token, {type_identifier, TokenLine, TokenCol, "float"}}.
string : {token, {type_identifier, TokenLine, TokenCol, "string"}}.
boolean : {token, {type_identifier, TokenLine, TokenCol, "boolean"}}.
atom : {token, {type_identifier, TokenLine, TokenCol, "atom"}}.
any : {token, {type_identifier, TokenLine, TokenCol, "any"}}.
fun : {token, {type_identifier, TokenLine, TokenCol, "fun"}}.
list : {token, {type_identifier, TokenLine, TokenCol, "list"}}.
tuple : {token, {type_identifier, TokenLine, TokenCol, "tuple"}}.
number : {token, {type_identifier, TokenLine, TokenCol, "number"}}.

% Identificadores e underscore
_ : {token, {underscore, TokenLine, TokenCol}}.
{L}({L}|{D})* : {token, {ident, TokenLine, TokenCol, TokenChars}}.

% Operadores e pontuação
= : {token, {bind, TokenLine, TokenCol}}.
; : {token, {semicolon, TokenLine, TokenCol}}.
\( : {token, {'(', TokenLine, TokenCol}}.
\) : {token, {')', TokenLine, TokenCol}}.
\[ : {token, {'[', TokenLine, TokenCol}}.
\] : {token, {']', TokenLine, TokenCol}}.
\{ : {token, {'{', TokenLine, TokenCol}}.
\} : {token, {'}', TokenLine, TokenCol}}.
, : {token, {comma, TokenLine, TokenCol}}.
: : {token, {colon, TokenLine, TokenCol}}.
% : {token, {'%', TokenLine, TokenCol}}.

% Operadores aritméticos
\+ : {token, {'+', TokenLine, TokenCol}}.
- : {token, {'-', TokenLine, TokenCol}}.
\* : {token, {'*', TokenLine, TokenCol}}.
/ : {token, {'/', TokenLine, TokenCol}}.

% Operador de concatenação
\+\+ : {token, {'++', TokenLine, TokenCol}}.

% List literals
\[ : {token, {'[', TokenLine, TokenCol}}.
\] : {token, {']', TokenLine, TokenCol}}.

% Whitespace e comentários
{WS}+ : skip_token.
{NL} : skip_token.
#.* : skip_token.

Erlang code.

strip_quotes([$" | Rest]) ->
    case lists:reverse(Rest) of
        [$" | Content] -> lists:reverse(Content);
        _ -> Rest
    end.

strip_colon([$: | Rest]) ->
    list_to_atom(Rest).