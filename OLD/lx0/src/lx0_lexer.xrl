Definitions.

D = [0-9]
L = [a-zA-Z_]
H = [a-fA-F0-9]
E = [Ee][+-]?{D}+
WS = [\s\t\r\n]
COMMENT = #[^\n]*

Rules.

%% Comments
{COMMENT} : skip_token.

%% Whitespace
{WS}+ : skip_token.

%% Keywords
def : {token, {'def', TokenLine}}.
defp : {token, {'defp', TokenLine}}.
end : {token, {'end', TokenLine}}.
do : {token, {'do', TokenLine}}.
record : {token, {'record', TokenLine}}.
type : {token, {'type', TokenLine}}.
case : {token, {'case', TokenLine}}.
if : {token, {'if', TokenLine}}.
else : {token, {'else', TokenLine}}.
when : {token, {'when', TokenLine}}.
receive : {token, {'receive', TokenLine}}.
after : {token, {'after', TokenLine}}.
fn : {token, {'fn', TokenLine}}.
for : {token, {'for', TokenLine}}.
with : {token, {'with', TokenLine}}.
match : {token, {'match', TokenLine}}.
rescue : {token, {'rescue', TokenLine}}.
application : {token, {'application', TokenLine}}.
supervisor : {token, {'supervisor', TokenLine}}.
worker : {token, {'worker', TokenLine}}.
strategy : {token, {'strategy', TokenLine}}.
children : {token, {'children', TokenLine}}.
deps : {token, {'deps', TokenLine}}.
describe : {token, {'describe', TokenLine}}.
test : {token, {'test', TokenLine}}.
assert : {token, {'assert', TokenLine}}.
and : {token, {'and', TokenLine}}.
or : {token, {'or', TokenLine}}.

%% Variables (start with lowercase or underscore, but not keywords)
([a-z_]({L}|{D})*) : {token, {var, TokenLine, list_to_atom(TokenChars)}}.

%% Atoms
:({L}({L}|{D})*) : {token, {atom, TokenLine, list_to_atom(string:substr(TokenChars, 2))}}.

%% Integers
{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

%% Floats
{D}+\.{D}+({E})? : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{D}+{E} : {token, {float, TokenLine, list_to_float(TokenChars)}}.

%% Strings
\"([^\"\\]|\\.)*\" : {token, {string, TokenLine, string:substr(TokenChars, 2, length(TokenChars) - 2)}}.

%% Binaries
<< : {token, {'<<', TokenLine}}.
>> : {token, {'>>', TokenLine}}.

%% Lists and tuples
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.

%% Maps
%\{ : {token, {'%{', TokenLine}}.
% : {token, {'%', TokenLine}}.

%% Records
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.

%% Operators
\+\+ : {token, {'++', TokenLine}}.
\-\- : {token, {'--', TokenLine}}.
== : {token, {'==', TokenLine}}.
!= : {token, {'!=', TokenLine}}.
<= : {token, {'<=', TokenLine}}.
>= : {token, {'>=', TokenLine}}.
= : {token, {'=', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.
\* : {token, {'*', TokenLine}}.
\/ : {token, {'/', TokenLine}}.
\> : {token, {'>', TokenLine}}.
\< : {token, {'<', TokenLine}}.
! : {token, {'!', TokenLine}}.
\| : {token, {'|', TokenLine}}.
\& : {token, {'&', TokenLine}}.
\^ : {token, {'^', TokenLine}}.
\~ : {token, {'~', TokenLine}}.
\? : {token, {'?', TokenLine}}.
-> : {token, {'->', TokenLine}}.
<- : {token, {'<-', TokenLine}}.

%% Type annotations
:: : {token, {'::', TokenLine}}.

%% Punctuation
\. : {token, {'.', TokenLine}}.
, : {token, {',', TokenLine}}.
; : {token, {';', TokenLine}}.
: : {token, {':', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
\#\{ : {token, {'#{', TokenLine}}.

%% Fat arrow for maps
=> : {token, {'=>', TokenLine}}.

%% Pipe operator
\|> : {token, {'|>', TokenLine}}.

%% List cons
\| : {token, {'|', TokenLine}}.

Erlang code.

-export([tokenize/1]).

tokenize(String) ->
    case string(String) of
        {ok, Tokens, _} ->
            {ok, Tokens};
        {error, ErrorInfo, _} ->
            {error, ErrorInfo}
    end.