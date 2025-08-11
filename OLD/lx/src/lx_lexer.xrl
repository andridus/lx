Definitions.

D = [0-9]
L = [a-zA-Z_]
WS = [\s\t\r\n]
COMMENT = #[^\n]*
STRING = \"[^\"]*\"
CHARLIST = \'[^\']*\'
ATOM = :({L}({L}|{D})*)
ATOM_LIST = :'[^']*'
KEY = ({L}({L}|{D})*):
CONST = @({L}({L}|{D})*)
IDENT = ({L}({L}|{D})*)
OP = (::|\|>|\+\+|\-\-|<>|\\\\|//|<=|>=|[+\-*=<>/~!])
DBLARROW = =>
ARROW_L = <-
ARROW_R = ->

Rules.

{COMMENT} : skip_token.
{WS}+ : skip_token.

defmacro : {token, {defmacro, TokenLine}}.
do : {token, {do, TokenLine}}.
end : {token, {end_, TokenLine}}.
with : {token, {with, TokenLine}}.
match : {token, {match, TokenLine}}.
case : {token, {case_, TokenLine}}.
rescue : {token, {rescue, TokenLine}}.
else : {token, {else_, TokenLine}}.

{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.

{STRING} : {token, {string, TokenLine, string:slice(TokenChars, 1, length(TokenChars) - 2)}}.
{ATOM_LIST} : {token, {atom, TokenLine, string:slice(TokenChars, 1, length(TokenChars) - 1)}}.
{ATOM} : {token, {atom, TokenLine, list_to_atom(string:slice(TokenChars, 1, length(TokenChars) - 1))}}.
{CHARLIST} : {token, {charlist, TokenLine, TokenChars}}.
{CONST} : {token, {const, TokenLine, string:slice(TokenChars, 1)}}.
{KEY} : {token, {key, TokenLine, list_to_atom(string:slice(TokenChars, 0, length(TokenChars) - 1))}}.
{IDENT} : {token, {ident, TokenLine, TokenChars}}.
{IDENT}:{IDENT} :
    {token, {mod_fun, TokenLine,
      {list_to_atom(hd(string:tokens(TokenChars, ":"))),
       list_to_atom(hd(tl(string:tokens(TokenChars, ":"))))}}}.
{ARROW_R} : {token, {arrow_r, TokenLine, TokenChars}}.
{ARROW_L} : {token, {arrow_l, TokenLine, TokenChars}}.
{DBLARROW} : {token, {dbl_arrow, TokenLine, TokenChars}}.
{OP} : {token, {operator, TokenLine, TokenChars}}.

\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
\% : {token, {'%', TokenLine}}.
\. : {token, {'.', TokenLine}}.
, : {token, {',', TokenLine}}.
: : {token, {':', TokenLine}}.
; : {token, {';', TokenLine}}.

Erlang code.