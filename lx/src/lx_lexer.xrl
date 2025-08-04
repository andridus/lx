Definitions.

D = [0-9]
L = [a-zA-Z_]
WS = [\s\t\r\n]
COMMENT = #[^\n]*
STRING = \"[^\"]*\"
ATOM = \'[^\']*\'

Rules.

{COMMENT} : skip_token.
{WS}+ : skip_token.

defmacro : {token, {defmacro, TokenLine}}.
do : {token, {do, TokenLine}}.
end : {token, {end_, TokenLine}}.
infix : {token, {infix, TokenLine}}.

{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.

{STRING} : {token, {string, TokenLine, string:slice(TokenChars, 1, length(TokenChars) - 2)}}.
{ATOM} : {token, {atom, TokenLine, list_to_atom(string:slice(TokenChars, 1, length(TokenChars) - 2))}}.

:({L}({L}|{D})*) : {token, {atom, TokenLine, list_to_atom(string:slice(TokenChars, 1))}}.
({L}({L}|{D})*) : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
, : {token, {',', TokenLine}}.
: : {token, {':', TokenLine}}.
; : {token, {';', TokenLine}}.

\% : {token, {'%', TokenLine}}.

\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.
\* : {token, {'*', TokenLine}}.
\/ : {token, {'/', TokenLine}}.
= : {token, {'=', TokenLine}}.
== : {token, {'==', TokenLine}}.
!= : {token, {'!=', TokenLine}}.

Erlang code.