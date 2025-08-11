%% Funções customizadas para o lexer
%% Este arquivo é incluído automaticamente após a geração do lexer

%% Tokenize source code
tokenize(Source) ->
    tokenize(Source, []).

tokenize(Source, _Options) ->
    case string(Source) of
        {ok, Tokens, _} ->
            {ok, Tokens};
        {error, ErrorInfo, _} ->
            {error, {lexical_error, ErrorInfo}}
    end.