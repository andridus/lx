-module(lx_compiler).
-export([compile/1, compile_file/1]).

compile_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            compile(binary_to_list(Content));
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

compile(Source) ->
    case lx_lexer:string(Source) of
        {ok, Tokens, _} when Tokens =:= [] ->
            {error, empty_source};
        {ok, Tokens, _} ->
            case lx_parser:parse(Tokens) of
                {ok, AST} ->
                    % Process macros
                    ExpandedAST = lx_macros:expand_macros(AST),
                    {ok, ExpandedAST};
                {error, {Line, Module, Message}} ->
                    {error, {parse_error, Line, Module, Message}}
            end;
        {error, {Line, Module, Message}, _} ->
            {error, {lexer_error, Line, Module, Message}}
    end.