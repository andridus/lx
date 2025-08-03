-module(lx2_parser).

-export([parse/1]).

parse(Tokens) ->
    case parse_program(Tokens) of
        {ok, AST, []} ->
            {ok, AST};
        {ok, AST, Rest} ->
            {error, {syntax_error, "Unexpected tokens: " ++ lists:flatten(io_lib:format("~p", [Rest]))}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_program(Tokens) ->
    parse_function_defs(Tokens, []).

parse_function_defs([], Acc) ->
    {ok, lists:reverse(Acc), []};
parse_function_defs(Tokens, Acc) ->
    case parse_function_def(Tokens) of
        {ok, FunDef, Rest} ->
            parse_function_defs(Rest, [FunDef | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

parse_function_def([{def, Line} | Rest]) ->
    case Rest of
        [{identifier, _, Name} | Rest1] ->
            case Rest1 of
                [{'(', _} | Rest2] ->
                    case Rest2 of
                        [{')', _} | Rest3] ->
                            case Rest3 of
                                [{do, _} | Rest4] ->
                                    case parse_block(Rest4) of
                                        {ok, Block, Rest5} ->
                                            % The block parser already consumed the 'end'
                                            {ok, {function_def, list_to_atom(Name), [], Block}, Rest5};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end;
                                _ ->
                                    {error, {syntax_error, "Expected 'do'"}}
                            end;
                        _ ->
                            {error, {syntax_error, "Expected ')'"}}
                    end;
                _ ->
                    {error, {syntax_error, "Expected '('"}}
            end;
        _ ->
            {error, {syntax_error, "Expected identifier after 'def'"}}
    end;
parse_function_def(_) ->
    {error, {syntax_error, "Expected 'def'"}}.

parse_block(Tokens) ->
    parse_statements(Tokens, []).

parse_statements([], Acc) ->
    {ok, lists:reverse(Acc), []};
parse_statements(Tokens, Acc) ->
    case parse_statement(Tokens) of
        {ok, Statement, Rest} ->
            case Rest of
                [{semicolon, _} | Rest1] ->
                    parse_statements(Rest1, [Statement | Acc]);
                [{'end', _} | Rest1] ->
                    % Found end, return the block
                    {ok, lists:reverse([Statement | Acc]), Rest1};
                _ ->
                    % No semicolon or end, continue parsing
                    parse_statements(Rest, [Statement | Acc])
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_statement(Tokens) ->
    case parse_variable_binding(Tokens) of
        {ok, Binding, Rest} ->
            {ok, Binding, Rest};
        {error, _} ->
            case parse_expression(Tokens) of
                {ok, Expr, Rest} ->
                    {ok, Expr, Rest};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_variable_binding([{identifier, _, Name} | Rest]) ->
    case Rest of
        [{equals, _} | Rest1] ->
            case parse_expression(Rest1) of
                {ok, Expr, Rest2} ->
                    {ok, {variable_binding, list_to_atom(Name), Expr}, Rest2};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, {syntax_error, "Expected '=' after identifier"}}
    end;
parse_variable_binding(_) ->
    {error, {syntax_error, "Expected identifier"}}.

parse_expression(Tokens) ->
    case parse_literal(Tokens) of
        {ok, Literal, Rest} ->
            {ok, Literal, Rest};
        {error, _} ->
            case parse_identifier(Tokens) of
                {ok, Id, Rest} ->
                    {ok, {variable_ref, Id}, Rest};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_literal([{integer, _, Value} | Rest]) ->
    {ok, {literal, integer, Value}, Rest};
parse_literal([{float, _, Value} | Rest]) ->
    {ok, {literal, float, Value}, Rest};
parse_literal([{string, _, Value} | Rest]) ->
    {ok, {literal, string, Value}, Rest};
parse_literal([{atom, _, Value} | Rest]) ->
    {ok, {literal, atom, Value}, Rest};
parse_literal([{boolean, _, Value} | Rest]) ->
    {ok, {literal, boolean, Value}, Rest};
parse_literal([{nil, _, Value} | Rest]) ->
    {ok, {literal, nil, Value}, Rest};
parse_literal([{underscore, _} | Rest]) ->
    {ok, {literal, undefined, undefined}, Rest};
parse_literal(_) ->
    {error, {syntax_error, "Expected literal"}}.

parse_identifier([{identifier, _, Name} | Rest]) ->
    {ok, list_to_atom(Name), Rest};
parse_identifier(_) ->
    {error, {syntax_error, "Expected identifier"}}.