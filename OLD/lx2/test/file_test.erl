-module(file_test).

-include_lib("eunit/include/eunit.hrl").

%% Test reading and parsing a .lx file
file_parse_test() ->
    Filename = "examples/test_simple.lx",
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            io:format("Source: ~s~n", [Source]),
            case lx2:parse(Source) of
                {ok, AST} ->
                    io:format("AST: ~p~n", [AST]),
                    ?assert(is_list(AST)),
                    ?assert(length(AST) > 0),
                    % Check that we have 5 statements
                    ?assertEqual(5, length(AST));
                {error, Error} ->
                    io:format("Parse error: ~p~n", [Error]),
                    ?assert(false)
            end;
        {error, Reason} ->
            io:format("File read error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% Test basic compilation
file_compile_test() ->
    Filename = "examples/test_simple.lx",
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            case lx2:compile(Source, #{mode => ast}) of
                {ok, AST} ->
                    io:format("Compiled AST: ~p~n", [AST]),
                    ?assert(is_list(AST)),
                    ?assert(length(AST) > 0);
                {error, Error} ->
                    io:format("Compile error: ~p~n", [Error]),
                    ?assert(false)
            end;
        {error, Reason} ->
            io:format("File read error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% Test with a more complex file
complex_file_test() ->
    % Create a temporary complex file
    ComplexSource = "
# Complex test file
def answer() do
    42
end

x = 10
y = 20
result = answer()
",
    TempFile = "examples/temp_complex.lx",
    ok = file:write_file(TempFile, ComplexSource),

    try
        case file:read_file(TempFile) of
            {ok, Content} ->
                Source = binary_to_list(Content),
                case lx2:parse(Source) of
                    {ok, AST} ->
                        io:format("Complex AST: ~p~n", [AST]),
                        ?assert(is_list(AST)),
                        ?assert(length(AST) > 0);
                    {error, Error} ->
                        io:format("Parse error: ~p~n", [Error]),
                        ?assert(false)
                end;
            {error, Reason} ->
                io:format("File read error: ~p~n", [Reason]),
                ?assert(false)
        end
    after
        % Clean up
        file:delete(TempFile)
    end.

%% Test error handling with invalid file
invalid_file_test() ->
    InvalidSource = "invalid syntax here",
    case lx2:parse(InvalidSource) of
        {error, _Error} ->
            % Expected error
            ?assert(true);
        {ok, _AST} ->
            % Unexpected success
            ?assert(false)
    end.