-module(lx2_cli).

-export([main/1]).

%% Main entry point for the CLI
main(Args) ->
    case parse_args(Args) of
        {compile, FilePath} ->
            compile_file(FilePath);
        {run, FilePath} ->
            run_file(FilePath);
        {ast, FilePath} ->
            show_ast(FilePath);
        {error, Message} ->
            print_usage(Message),
            halt(1);
        help ->
            print_usage(),
            halt(0)
    end.

%% Parse command line arguments
parse_args([]) ->
    {error, "No arguments provided"};
parse_args(["help" | _]) ->
    help;
parse_args(["--help" | _]) ->
    help;
parse_args(["-h" | _]) ->
    help;
parse_args([FilePath]) ->
    % Default to compile if no command specified
    {compile, FilePath};
parse_args(["compile", FilePath]) ->
    {compile, FilePath};
parse_args(["run", FilePath]) ->
    {run, FilePath};
parse_args(["ast", FilePath]) ->
    {ast, FilePath};
parse_args(_) ->
    {error, "Invalid arguments"}.

%% Compile a .lx file to .erl
compile_file(FilePath) ->
    case read_file(FilePath) of
        {ok, Source} ->
            % Extract module name from file path
            ModuleName = extract_module_name_from_path(FilePath),
            case lx2:compile(Source, #{mode => erl, module_name => ModuleName}) of
                {ok, ErlCode} ->
                    ErlFilePath = get_erl_path(FilePath),
                    case file:write_file(ErlFilePath, ErlCode) of
                        ok ->
                            io:format("Compiled ~s to ~s~n", [FilePath, ErlFilePath]),
                            {ok, ErlFilePath};
                        {error, Reason} ->
                            io:format("Error writing file ~s: ~p~n", [ErlFilePath, Reason]),
                            {error, Reason}
                    end;
                {error, Error} ->
                    print_error(FilePath, Source, Error),
                    {error, Error}
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [FilePath, Reason]),
            {error, Reason}
    end.

%% Run a .lx file directly
run_file(FilePath) ->
    case read_file(FilePath) of
        {ok, Source} ->
            % Extract module name from file path
            ModuleName = extract_module_name_from_path(FilePath),
            case lx2:compile(Source, #{module_name => ModuleName}) of
                {ok, ModuleName, BeamCode, _} ->
                    % Load the module
                    case code:load_binary(ModuleName, "", BeamCode) of
                        {module, ModuleName} ->
                            % Try to find a main function or the first function
                            case find_main_function(ModuleName) of
                                {ok, FunName} ->
                                    % Execute the function
                                    try
                                        Result = ModuleName:FunName(),
                                        io:format("~p~n", [Result])
                                    catch
                                        _:Error ->
                                            io:format("Runtime error: ~p~n", [Error]),
                                            {error, Error}
                                    end;
                                {error, Reason} ->
                                    io:format("No executable function found: ~p~n", [Reason]),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            io:format("Error loading module: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                {ModuleName, BeamCode, _} ->
                    % Load the module
                    case code:load_binary(ModuleName, "", BeamCode) of
                        {module, ModuleName} ->
                            % Try to find a main function or the first function
                            case find_main_function(ModuleName) of
                                {ok, FunName} ->
                                    % Execute the function
                                    try
                                        Result = ModuleName:FunName(),
                                        io:format("~p~n", [Result])
                                    catch
                                        _:Error ->
                                            io:format("Runtime error: ~p~n", [Error]),
                                            {error, Error}
                                    end;
                                {error, Reason} ->
                                    io:format("No executable function found: ~p~n", [Reason]),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            io:format("Error loading module: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                {error, Error} ->
                    print_error(FilePath, Source, Error),
                    {error, Error}
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [FilePath, Reason]),
            {error, Reason}
    end.

%% Show AST for a .lx file
show_ast(FilePath) ->
    case read_file(FilePath) of
        {ok, Source} ->
            case lx2:compile(Source, #{mode => ast}) of
                {ok, AST} ->
                    io:format("AST for ~s:~n", [FilePath]),
                    print_ast(AST, 0);
                {error, Error} ->
                    print_error(FilePath, Source, Error),
                    {error, Error}
            end;
        {error, Reason} ->
            io:format("Error reading file ~s: ~p~n", [FilePath, Reason]),
            {error, Reason}
    end.

%% Read file content
read_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            {ok, binary_to_list(Content)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get .erl file path from .lx file path
get_erl_path(LxFilePath) ->
    case filename:extension(LxFilePath) of
        ".lx" ->
            filename:rootname(LxFilePath) ++ ".erl";
        _ ->
            LxFilePath ++ ".erl"
    end.

%% Extract module name from file path
extract_module_name_from_path(FilePath) ->
    % Get the filename without extension
    Filename = filename:basename(FilePath, ".lx"),
    % Convert to atom (Erlang module name)
    list_to_atom(Filename).

%% Find a function to execute (main or first function)
find_main_function(ModuleName) ->
    % Try to get module info
    case code:is_loaded(ModuleName) of
        {file, _} ->
            % Try main function first
            case erlang:function_exported(ModuleName, main, 0) of
                true ->
                    {ok, main};
                false ->
                    % Try to find any function with arity 0
                    case find_zero_arity_function(ModuleName) of
                        {ok, FunName} ->
                            {ok, FunName};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        false ->
            {error, "Module not loaded"}
    end.

%% Find a function with arity 0
find_zero_arity_function(ModuleName) ->
    % This is a simplified approach - in a real implementation,
    % you might want to parse the AST to find functions
    % For now, we'll try common function names and the module name itself
    CommonNames = [answer, test, main, start, init, simple, ModuleName],
    find_function_in_list(ModuleName, CommonNames).

%% Try to find a function from a list of names
find_function_in_list(_ModuleName, []) ->
    {error, "No suitable function found"};
find_function_in_list(ModuleName, [FunName | Rest]) ->
    case erlang:function_exported(ModuleName, FunName, 0) of
        true ->
            {ok, FunName};
        false ->
            find_function_in_list(ModuleName, Rest)
    end.

%% Print AST in a formatted way
print_ast(AST, Indent) ->
    IndentStr = string:copies("  ", Indent),
    case AST of
        [] ->
            io:format("~s[]~n", [IndentStr]);
        [H | T] ->
            print_ast(H, Indent),
            case T of
                [] -> ok;
                _ -> print_ast(T, Indent)
            end;
        {function_def, Name, Params, Body} ->
            io:format("~s{function_def, ~p, ~p,~n", [IndentStr, Name, Params]),
            print_ast(Body, Indent + 1),
            io:format("~s}~n", [IndentStr]);
        {literal, Type, Value} ->
            io:format("~s{literal, ~p, ~p}~n", [IndentStr, Type, Value]);
        _ ->
            io:format("~s~p~n", [IndentStr, AST])
    end.

%% Print error with context
print_error(FilePath, Source, Error) ->
    case Error of
        {semantic_error, {undefined_variable, Var}} ->
            % Find the line where the undefined variable appears
            Lines = string:split(Source, "\n", all),
            Line = find_variable_line(Lines, Var),
            print_compilation_error(FilePath, Source, Line, "Type Error", "Undefined variable: " ++ atom_to_list(Var));
        {semantic_error, ErrorDetails} ->
            io:format("Compilation failed: [Semantic Error] ~s~n", [FilePath]),
            io:format("~p~n", [ErrorDetails]);
        {syntax_error, Line, Message} ->
            print_compilation_error(FilePath, Source, Line, "Syntax Error", Message);
        {syntax_error, {syntax_error, Line, Message}} ->
            print_compilation_error(FilePath, Source, Line, "Syntax Error", Message);
        {lexical_error, Line, Message} ->
            print_compilation_error(FilePath, Source, Line, "Lexical Error", Message);
        _ ->
            io:format("Compilation failed: [Unknown Error] ~p~n", [Error])
    end.

%% Find the line where a variable appears
find_variable_line(Lines, Var) ->
    VarStr = atom_to_list(Var),
    find_variable_line_rec(Lines, VarStr, 1).

find_variable_line_rec([], _VarStr, _LineNum) ->
    1; % Default to line 1 if not found
find_variable_line_rec([Line | Rest], VarStr, LineNum) ->
    case string:find(Line, VarStr) of
        nomatch ->
            find_variable_line_rec(Rest, VarStr, LineNum + 1);
        _ ->
            LineNum
    end.

%% Print compilation error with improved formatting and colors
print_compilation_error(FilePath, Source, Line, ErrorType, Message) ->
    Lines = string:split(Source, "\n", all),

    % ANSI color codes
    Red = "\033[31m",
    Yellow = "\033[33m",
    Blue = "\033[34m",
    Cyan = "\033[36m",
    Reset = "\033[0m",

    % Format error message with colors
    case ErrorType of
        "Lexical Error" ->
            io:format("~sCompilation failed:~s [~sLexical Error~s] ~s~s:~p:~p~s~n",
                     [Red, Reset, Yellow, Reset, Cyan, FilePath, Line, 1, Reset]),
            case string:find(Message, "{illegal,\"") of
                nomatch ->
                    io:format("~s~s~n", [Message, Reset]);
                _ ->
                    % Extract character from message like "{illegal,\"@\"}"
                    case re:run(Message, "\\{illegal,\"([^\"]+)\"\\}", [{capture, all_but_first, list}]) of
                        {match, [Char]} ->
                            io:format("~sIllegal character: ~s~s~n", [Yellow, Char, Reset]);
                        _ ->
                            io:format("~s~s~n", [Message, Reset])
                    end
            end;
        "Syntax Error" ->
            io:format("~sCompilation failed:~s [~sSyntax Error~s] ~s~s:~p:~p~s~n",
                     [Red, Reset, Yellow, Reset, Cyan, FilePath, Line, 1, Reset]),
            io:format("~s~s~n", [Message, Reset]);
                "Type Error" ->
            io:format("~sCompilation failed:~s [~sAnalysis Error~s] ~s:~p:~p~s~n",
                     [Red, Reset, Yellow, Reset, FilePath, Line, 1, Reset]),
            io:format("~s~s~n", [Message, Reset]);
        _ ->
            io:format("~sCompilation failed:~s [~s~s~s] ~s:~p:~p~s~n",
                     [Red, Reset, Yellow, ErrorType, Reset, FilePath, Line, 1, Reset]),
            io:format("~s~s~n", [Message, Reset])
    end,

        % Show code snippet with line numbers and colors (like V)
    % Show context lines (2 before, current line, 2 after)
    StartLine = max(1, Line - 2),
    EndLine = min(length(Lines), Line + 2),

    [begin
        LineNum = I,
        LineContent = lists:nth(I, Lines),
        case LineNum of
            Line ->
                % Current line with error
                ErrorPos = find_error_position(LineContent, Message, ErrorType),
                io:format("~s~4..0B~s | ~s~s~n", [Red, LineNum, Reset, LineContent, Reset]),
                % Error indicator
                Indicator = string:copies(" ", ErrorPos) ++ "^~~~",
                io:format("     | ~s~s~s~n", [Red, Indicator, Reset]);
            _ ->
                % Other lines
                io:format("~s~4..0B~s | ~s~s~n", [Blue, LineNum, Reset, LineContent, Reset])
        end
    end || I <- lists:seq(StartLine, EndLine)].

%% Find error position in line
find_error_position(LineContent, Message, ErrorType) ->
    case ErrorType of
        "Lexical Error" ->
            % For lexical errors, find the illegal character
            case string:find(Message, "{illegal,\"") of
                nomatch ->
                    % Fallback to end of line
                    string:length(LineContent);
                _ ->
                    case re:run(Message, "\\{illegal,\"([^\"]+)\"\\}", [{capture, all_but_first, list}]) of
                        {match, [Char]} ->
                            % Find the position of the illegal character
                            find_char_position(LineContent, Char);
                        _ ->
                            string:length(LineContent)
                    end
            end;
        "Type Error" ->
            % For type errors with undefined variable, find the variable name
            case string:find(Message, "Undefined variable: ") of
                nomatch ->
                    string:length(LineContent);
                _ ->
                    % Extract variable name from message like "Undefined variable: y"
                    case re:run(Message, "Undefined variable: ([a-zA-Z_][a-zA-Z0-9_]*)", [{capture, all_but_first, list}]) of
                        {match, [VarName]} ->
                            find_char_position(LineContent, VarName);
                        _ ->
                            string:length(LineContent)
                    end
            end;
        "Syntax Error" ->
            % For syntax errors, try to find the problematic token
            case string:find(Message, "syntax error before: ") of
                nomatch ->
                    string:length(LineContent);
                _ ->
                    % Extract token from message like "syntax error before: def"
                    case re:run(Message, "syntax error before: ([a-zA-Z_]+)", [{capture, all_but_first, list}]) of
                        {match, [Token]} ->
                            find_token_position(LineContent, Token);
                        _ ->
                            string:length(LineContent)
                    end
            end;
        _ ->
            string:length(LineContent)
    end.

%% Find position of a specific character in line
find_char_position(LineContent, Char) ->
    case string:find(LineContent, Char) of
        nomatch ->
            0;
        Found ->
            % Calculate position: length of string before the found character
            string:length(LineContent) - string:length(Found) + 1
    end.

%% Find position of a token in line
find_token_position(LineContent, Token) ->
    case string:find(LineContent, Token) of
        nomatch ->
            string:length(LineContent);
        _ ->
            % Find the start position of the token
            find_token_start(LineContent, Token, 1)
    end.

find_token_start([], _Token, _Pos) ->
    0;
find_token_start(LineContent, Token, Pos) ->
    case string:find(LineContent, Token) of
        nomatch ->
            Pos;
        _ ->
            % Check if this is the start of the token (not part of another word)
            TokenLength = string:length(Token),
            LineLength = string:length(LineContent),
            if
                Pos + TokenLength - 1 =< LineLength ->
                    % Check if it's a word boundary
                    case Pos of
                        1 -> % Start of line
                            Pos;
                        _ ->
                            % Check if previous char is whitespace or special
                            PrevChar = lists:nth(Pos - 1, LineContent),
                            case is_word_boundary(PrevChar) of
                                true -> Pos;
                                false -> find_token_start(string:slice(LineContent, 1), Token, Pos + 1)
                            end
                    end;
                true ->
                    Pos
            end
    end.

%% Check if character is a word boundary
is_word_boundary(Char) ->
    lists:member(Char, [$\s, $\t, $., $,, $;, $:, $!, $?, $), $}, $], $>]).

%% Print usage information
print_usage() ->
    print_usage("").
print_usage(Message) ->
    case Message of
        "" -> ok;
        _ -> io:format("Error: ~s~n~n", [Message])
    end,
    io:format("LX2 Compiler - Usage:~n~n"),
    io:format("  lx <file.lx>           Compile .lx file to .erl~n"),
    io:format("  lx compile <file.lx>   Compile .lx file to .erl~n"),
    io:format("  lx run <file.lx>       Compile and run .lx file~n"),
    io:format("  lx ast <file.lx>       Show AST (Abstract Syntax Tree)~n"),
    io:format("  lx help                Show this help message~n"),
    io:format("  lx --help              Show this help message~n"),
    io:format("  lx -h                  Show this help message~n~n"),
    io:format("Examples:~n"),
    io:format("  lx examples/task_01/simple.lx~n"),
    io:format("  lx run examples/task_01/simple.lx~n"),
    io:format("  lx compile examples/task_01/simple.lx~n"),
    io:format("  lx ast examples/task_01/simple.lx~n~n"),
    io:format("The compiler will:~n"),
    io:format("  - Parse the .lx file~n"),
    io:format("  - Generate Erlang code~n"),
    io:format("  - Save as .erl file (compile mode)~n"),
    io:format("  - Execute directly (run mode)~n"),
    io:format("  - Show AST structure (ast mode)~n").