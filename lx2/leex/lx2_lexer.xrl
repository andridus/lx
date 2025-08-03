Definitions.

D = [0-9]
L = [a-zA-Z_]
WS = [\s\t]
NL = \n|\r\n|\r

Rules.

{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
\"[^\"]*\" : {token, {string, TokenLine, strip_quotes(TokenChars)}}.
:({L}({L}|{D})*) : {token, {atom, TokenLine, strip_colon(TokenChars)}}.
true : {token, {boolean, TokenLine, true}}.
false : {token, {boolean, TokenLine, false}}.
nil : {token, {nil, TokenLine, nil}}.
def : {token, {def, TokenLine}}.
do : {token, {do, TokenLine}}.
end : {token, {'end', TokenLine}}.
_ : {token, {underscore, TokenLine}}.
{L}({L}|{D})* : {token, {identifier, TokenLine, TokenChars}}.
= : {token, {equals, TokenLine}}.
; : {token, {semicolon, TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
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