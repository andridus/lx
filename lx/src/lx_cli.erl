-module(lx_cli).
-export([main/1, compile_file/1, read_file/1]).

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
    % Default to run if no command specified
    {run, FilePath};
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
            case lx_compiler:compile(Source, #{mode => erl, module_name => ModuleName}) of
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
            case lx_compiler:compile(Source, #{module_name => ModuleName}) of
                {ok, BeamCode} ->
                    % Load the module
                    case code:load_binary(ModuleName, "", BeamCode) of
                        {module, ModuleName} ->
                            % Execute the main function
                            try
                                Result = ModuleName:main(),
                                io:format("~p~n", [Result])
                            catch
                                _:Error ->
                                    io:format("Runtime error: ~p~n", [Error]),
                                    {error, Error}
                            end;
                        {error, Reason} ->
                            io:format("Error loading module: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                {ok, ModuleName, BeamCode, _} ->
                    % Load the module
                    case code:load_binary(ModuleName, "", BeamCode) of
                        {module, ModuleName} ->
                            % Execute the main function
                            try
                                Result = ModuleName:main(),
                                io:format("~p~n", [Result])
                            catch
                                _:Error ->
                                    io:format("Runtime error: ~p~n", [Error]),
                                    {error, Error}
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
            case lx_compiler:compile(Source, #{mode => ast}) of
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
        {integer, Line, Value} ->
            io:format("~s{integer, ~p, ~p}~n", [IndentStr, Line, Value]);
        {float, Line, Value} ->
            io:format("~s{float, ~p, ~p}~n", [IndentStr, Line, Value]);
        {string, Line, Value} ->
            io:format("~s{string, ~p, ~p}~n", [IndentStr, Line, Value]);
        {atom, Line, Value} ->
            io:format("~s{atom, ~p, ~p}~n", [IndentStr, Line, Value]);
        {ident, Line, Value} ->
            io:format("~s{ident, ~p, ~p}~n", [IndentStr, Line, Value]);
        {tuple, Line, Elements} ->
            io:format("~s{tuple, ~p,~n", [IndentStr, Line]),
            [print_ast(Elem, Indent + 1) || Elem <- Elements],
            io:format("~s}~n", [IndentStr]);
        {list, Line, Elements} ->
            io:format("~s{list, ~p,~n", [IndentStr, Line]),
            [print_ast(Elem, Indent + 1) || Elem <- Elements],
            io:format("~s}~n", [IndentStr]);
        {map, Line, Entries} ->
            io:format("~s{map, ~p,~n", [IndentStr, Line]),
            [print_ast(Entry, Indent + 1) || Entry <- Entries],
            io:format("~s}~n", [IndentStr]);
        {map_entry, Line, Key, Value} ->
            io:format("~s{map_entry, ~p,~n", [IndentStr, Line]),
            print_ast(Key, Indent + 1),
            print_ast(Value, Indent + 1),
            io:format("~s}~n", [IndentStr]);
        {binary_op, Line, Left, Op, Right} ->
            io:format("~s{binary_op, ~p,~n", [IndentStr, Line]),
            print_ast(Left, Indent + 1),
            io:format("~s  ~p,~n", [IndentStr, Op]),
            print_ast(Right, Indent + 1),
            io:format("~s}~n", [IndentStr]);
        {macro_def, Line, Name, Params, Body} ->
            io:format("~s{macro_def, ~p, ~p, ~p,~n", [IndentStr, Line, Name, Params]),
            print_ast(Body, Indent + 1),
            io:format("~s}~n", [IndentStr]);
        {macro_def_infix, Line, Name, Params, Body} ->
            io:format("~s{macro_def_infix, ~p, ~p, ~p,~n", [IndentStr, Line, Name, Params]),
            print_ast(Body, Indent + 1),
            io:format("~s}~n", [IndentStr]);
        {do_block, Line, ExpressionList} ->
            io:format("~s{do_block, ~p,~n", [IndentStr, Line]),
            [print_ast(Expr, Indent + 1) || Expr <- ExpressionList],
            io:format("~s}~n", [IndentStr]);
        _ ->
            io:format("~s~p~n", [IndentStr, AST])
    end.

%% Print error with context
print_error(FilePath, Source, Error) ->
    case Error of
        {lexical_error, Line, Message} ->
            print_compilation_error(FilePath, Source, Line, "Lexical Error", Message);
        {parse_error, Line, _Module, Message} ->
            print_compilation_error(FilePath, Source, Line, "Parse Error", Message);
        {syntax_error, Line, Message} ->
            print_compilation_error(FilePath, Source, Line, "Syntax Error", Message);
        {macro_error, Line, _Module, Message} ->
            print_compilation_error(FilePath, Source, Line, "Macro Error", Message);
        _ ->
            io:format("Compilation failed: [Unknown Error] ~p~n", [Error])
    end.

%% Print compilation error with improved formatting
print_compilation_error(FilePath, Source, Line, ErrorType, Message) ->
    Lines = string:split(Source, "\n", all),

    % Format error message
    io:format("Compilation failed: [~s] ~s:~p:~p~n", [ErrorType, FilePath, Line, 1]),
    io:format("~s~n", [Message]),

    % Show code snippet with line numbers
    StartLine = max(1, Line - 2),
    EndLine = min(length(Lines), Line + 2),

    [begin
        LineNum = I,
        LineContent = lists:nth(I, Lines),
        case LineNum of
            Line ->
                % Current line with error
                io:format("~4..0B | ~s~n", [LineNum, LineContent]);
            _ ->
                % Other lines
                io:format("~4..0B | ~s~n", [LineNum, LineContent])
        end
    end || I <- lists:seq(StartLine, EndLine)].

%% Print usage information
print_usage() ->
    print_usage("").
print_usage(Message) ->
    case Message of
        "" -> ok;
        _ -> io:format("Error: ~s~n~n", [Message])
    end,
    io:format("LX Compiler - Usage:~n~n"),
    io:format("  lx <file.lx>           Run .lx file directly~n"),
    io:format("  lx run <file.lx>       Run .lx file directly~n"),
    io:format("  lx compile <file.lx>   Compile .lx file to .erl~n"),
    io:format("  lx ast <file.lx>       Show AST (Abstract Syntax Tree)~n"),
    io:format("  lx help                Show this help message~n"),
    io:format("  lx --help              Show this help message~n"),
    io:format("  lx -h                  Show this help message~n~n"),
    io:format("Examples:~n"),
    io:format("  lx examples/literals.lx~n"),
    io:format("  lx run examples/literals.lx~n"),
    io:format("  lx compile examples/literals.lx~n"),
    io:format("  lx ast examples/literals.lx~n~n"),
    io:format("The compiler will:~n"),
    io:format("  - Parse the .lx file~n"),
    io:format("  - Generate Erlang code~n"),
    io:format("  - Save as .erl file (compile mode)~n"),
    io:format("  - Execute directly (run mode)~n"),
    io:format("  - Show AST structure (ast mode)~n").