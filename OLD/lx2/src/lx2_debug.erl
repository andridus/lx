-module(lx2_debug).

-export([debug_parse/1]).

debug_parse(Tokens) ->
    io:format("=== DEBUG PARSER ===~n"),
    io:format("Tokens: ~p~n", [Tokens]),
    debug_parse_program(Tokens).

debug_parse_program(Tokens) ->
    io:format("Parsing PROGRAM~n"),
    case debug_parse_function_def(Tokens) of
        {ok, FunctionDef, Rest} ->
            io:format("FunctionDef parsed: ~p~n", [FunctionDef]),
            io:format("Rest tokens: ~p~n", [Rest]),
            {ok, [FunctionDef]};
        {error, Reason} ->
            io:format("ERROR in function_def: ~p~n", [Reason]),
            {error, Reason}
    end.

debug_parse_function_def([{def, _} | Rest]) ->
    io:format("Found 'def'~n"),
    case Rest of
        [{identifier, _, Name} | Rest2] ->
            io:format("Found identifier: ~p~n", [Name]),
            case Rest2 of
                [{'(', _} | Rest3] ->
                    io:format("Found '('~n"),
                    case Rest3 of
                        [{')', _} | Rest4] ->
                            io:format("Found ')'~n"),
                            case Rest4 of
                                [{do, _} | Rest5] ->
                                    io:format("Found 'do'~n"),
                                    case debug_parse_block(Rest5) of
                                        {ok, Block, Rest6} ->
                                            io:format("Block parsed: ~p~n", [Block]),
                                            case Rest6 of
                                                [{'end', _} | Rest7] ->
                                                    io:format("Found 'end'~n"),
                                                    FunctionDef = {function_def, Name, [], Block},
                                                    {ok, FunctionDef, Rest7};
                                                _ ->
                                                    io:format("ERROR: Expected 'end'~n"),
                                                    {error, "Expected 'end'"}
                                            end;
                                        {error, Reason} ->
                                            {error, Reason}
                                    end;
                                _ ->
                                    io:format("ERROR: Expected 'do'~n"),
                                    {error, "Expected 'do'"}
                            end;
                        _ ->
                            io:format("ERROR: Expected ')'~n"),
                            {error, "Expected ')'"}
                    end;
                _ ->
                    io:format("ERROR: Expected '('~n"),
                    {error, "Expected '('"}
            end;
        _ ->
            io:format("ERROR: Expected identifier~n"),
            {error, "Expected identifier"}
    end;
debug_parse_function_def(_) ->
    {error, "Expected 'def'"}.

debug_parse_block(Tokens) ->
    io:format("Parsing BLOCK~n"),
    case debug_parse_variable_binding(Tokens) of
        {ok, Binding, Rest} ->
            io:format("Variable binding parsed: ~p~n", [Binding]),
            % Se o próximo token é 'end', então o bloco termina aqui
            case Rest of
                [{'end', _} | _] ->
                    io:format("Block ends after binding~n"),
                    {ok, [Binding], Rest};
                _ ->
                    % Caso contrário, continua parsing recursivo
                    case debug_parse_block(Rest) of
                        {ok, RestBlock, Rest2} ->
                            {ok, [Binding | RestBlock], Rest2};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, _} ->
            case debug_parse_expression(Tokens) of
                {ok, Expr, Rest} ->
                    io:format("Expression parsed: ~p~n", [Expr]),
                    {ok, [Expr], Rest};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

debug_parse_variable_binding([{identifier, _, Name} | Rest]) ->
    io:format("Found identifier for binding: ~p~n", [Name]),
    case Rest of
        [{equals, _} | Rest2] ->
            io:format("Found '='~n"),
            case debug_parse_expression(Rest2) of
                {ok, Expr, Rest3} ->
                    io:format("Expression for binding parsed: ~p~n", [Expr]),
                    Binding = {variable_binding, Name, Expr},
                    {ok, Binding, Rest3};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            io:format("ERROR: Expected '=' after identifier~n"),
            {error, "Expected '='"}
    end;
debug_parse_variable_binding(_) ->
    {error, "Expected identifier for binding"}.

debug_parse_expression([{integer, _, Value} | Rest]) ->
    io:format("Found integer: ~p~n", [Value]),
    Expr = {literal, integer, Value},
    {ok, Expr, Rest};
debug_parse_expression([{identifier, _, Name} | Rest]) ->
    io:format("Found identifier for ref: ~p~n", [Name]),
    Expr = {variable_ref, Name},
    {ok, Expr, Rest};
debug_parse_expression(_) ->
    {error, "Expected expression"}.