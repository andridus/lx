-module(lx2_lexer).

-export([string/1, string/2]).

string(Source) ->
    string(Source, 1).

string(Source, StartLine) ->
    Tokens = tokenize(Source, StartLine, []),
    {ok, Tokens, StartLine}.

tokenize([], _Line, Acc) ->
    lists:reverse(Acc);
tokenize([Char | Rest], Line, Acc) ->
    case Char of
        $\s -> tokenize(Rest, Line, Acc);
        $\t -> tokenize(Rest, Line, Acc);
        $\n -> tokenize(Rest, Line + 1, Acc);
        $\r -> tokenize(Rest, Line, Acc);
        $# ->
            % Skip comments
            {CommentRest, _} = skip_comment(Rest),
            tokenize(CommentRest, Line, Acc);
        $; ->
            tokenize(Rest, Line, [{semicolon, Line} | Acc]);
        $= ->
            tokenize(Rest, Line, [{equals, Line} | Acc]);
        $( ->
            tokenize(Rest, Line, [{'(', Line} | Acc]);
        $) ->
            tokenize(Rest, Line, [{')', Line} | Acc]);
        $_ ->
            tokenize(Rest, Line, [{underscore, Line} | Acc]);
        $: ->
            {AtomRest, Atom} = read_atom(Rest),
            tokenize(AtomRest, Line, [{atom, Line, Atom} | Acc]);
        $" ->
            {StringRest, String} = read_string(Rest),
            tokenize(StringRest, Line, [{string, Line, String} | Acc]);
        $0 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $1 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $2 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $3 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $4 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $5 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $6 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $7 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $8 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        $9 ->
            {NumRest, Num} = read_number([Char | Rest]),
            Type = if is_float(Num) -> float; true -> integer end,
            tokenize(NumRest, Line, [{Type, Line, Num} | Acc]);
        _ when Char >= $a, Char =< $z; Char >= $A, Char =< $Z; Char =:= $_ ->
            {IdRest, Id} = read_identifier([Char | Rest]),
            Token = case Id of
                "def" -> {def, Line};
                "do" -> {do, Line};
                "end" -> {'end', Line};
                "true" -> {boolean, Line, true};
                "false" -> {boolean, Line, false};
                "nil" -> {nil, Line, nil};
                _ -> {identifier, Line, Id}
            end,
            tokenize(IdRest, Line, [Token | Acc])
    end.

skip_comment([]) ->
    {[], []};
skip_comment([$\n | Rest]) ->
    {Rest, []};
skip_comment([_ | Rest]) ->
    skip_comment(Rest).

read_atom(Chars) ->
    read_atom(Chars, []).

read_atom([], Acc) ->
    {[], list_to_atom(lists:reverse(Acc))};
read_atom([Char | Rest], Acc) when Char >= $a, Char =< $z; Char >= $A, Char =< $Z; Char >= $0, Char =< $9; Char =:= $_ ->
    read_atom(Rest, [Char | Acc]);
read_atom(Rest, Acc) ->
    {Rest, list_to_atom(lists:reverse(Acc))}.

read_string(Chars) ->
    read_string(Chars, []).

read_string([], _Acc) ->
    {[], ""};
read_string([$" | Rest], Acc) ->
    {Rest, lists:reverse(Acc)};
read_string([Char | Rest], Acc) ->
    read_string(Rest, [Char | Acc]).

read_number(Chars) ->
    read_number(Chars, []).

read_number([], Acc) ->
    {[], list_to_integer(lists:reverse(Acc))};
read_number([Char | Rest], Acc) when Char >= $0, Char =< $9 ->
    read_number(Rest, [Char | Acc]);
read_number([$. | Rest], Acc) ->
    read_float(Rest, [$. | Acc]);
read_number(Rest, Acc) ->
    {Rest, list_to_integer(lists:reverse(Acc))}.

read_float(Chars, Acc) ->
    read_float_impl(Chars, Acc).

read_float_impl([], Acc) ->
    {[], list_to_float(lists:reverse(Acc))};
read_float_impl([Char | Rest], Acc) when Char >= $0, Char =< $9 ->
    read_float_impl(Rest, [Char | Acc]);
read_float_impl(Rest, Acc) ->
    {Rest, list_to_float(lists:reverse(Acc))}.

read_identifier(Chars) ->
    read_identifier(Chars, []).

read_identifier([], Acc) ->
    {[], lists:reverse(Acc)};
read_identifier([Char | Rest], Acc) when Char >= $a, Char =< $z; Char >= $A, Char =< $Z; Char >= $0, Char =< $9; Char =:= $_ ->
    read_identifier(Rest, [Char | Acc]);
read_identifier(Rest, Acc) ->
    {Rest, lists:reverse(Acc)}.