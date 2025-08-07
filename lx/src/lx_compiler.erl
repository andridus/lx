-module(lx_compiler).
-export([compile/1, compile/2, compile_file/1, compile_file/2, code_generation/2, compile_ast/1, compile_file_ast/1]).

compile_file(Filename) ->
    compile_file(Filename, #{}).

compile_file(Filename, Options) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Source = binary_to_list(Content),
            ModuleName = extract_module_name_from_path(Filename),
            compile(Source, Options#{module_name => ModuleName});
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

% Convenience function to compile file and return only AST
compile_file_ast(Filename) ->
    compile_file(Filename, #{ast_only => true}).

compile(Source) ->
    compile(Source, #{}).

compile(Source, Options) ->
    % Set default module name if not provided
    OptionsWithModule = case maps:get(module_name, Options, undefined) of
        undefined -> Options#{module_name => test};
        _ -> Options
    end,
    % Phase 1: Lexical analysis
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            io:format("Tokens: ~p~n", [Tokens]),
            % Phase 2: Syntactic analysis
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    io:format("AST: ~p~n~n", [AST]),
                    % Phase 3: Macro expansion
                    case lx_macros:expand_macros(AST) of
                        {error, {undefined_macro, Line, Name, Arg}} ->
                            ErrorMsg = lists:flatten(io_lib:format("Undefined macro '~p' called with argument ~p", [Name, Arg])),
                            {error, {parse_error, Line, lx_parser, [ErrorMsg]}};
                        {error, Error} ->
                            {error, Error};
                        {ok, ExpandedAST} ->
                            % Check if we should return only AST
                            case maps:get(ast_only, Options, false) of
                                true ->
                                    {ok, ExpandedAST};
                                false ->
                                    % Phase 4: Code generation
                                    case lx_codegen:ast_to_erlang_module(ExpandedAST, maps:get(module_name, OptionsWithModule)) of
                                        {ok, ModuleName, ByteCode, SourceMap} ->
                                            {ok, ModuleName, ByteCode, SourceMap};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end
                            end
                    end;
                {error, Reason} ->
                    io:format("Reason: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Reason: ~p~n", [Reason]),
            {error, Reason}
    end.

% Convenience function to compile and return only AST
compile_ast(Source) ->
    compile(Source, #{ast_only => true}).

%% Lexical analysis phase
lexical_analysis(Source) ->
    case lx_lexer:string(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, {Line, Module, Message}, _EndLine} ->
            {error, {lexer_error, Line, Module, Message}};
        {error, Error, _EndLine} ->
            {error, Error}
    end.

%% Syntactic analysis phase
syntactic_analysis([]) -> {ok, []};
syntactic_analysis(Tokens) ->
    case lx_parser:parse(Tokens) of
        {ok, AST} ->
            {ok, AST};
        {error, {Line, Module, Message}} ->
            {error, {parse_error, Line, Module, Message}};
        {error, Error} ->
            {error, Error}
    end.

%% Code generation phase
code_generation(AST, Options) ->
    ModuleName = maps:get(module_name, Options, unknown),
    Mode = maps:get(mode, Options, both),

    case Mode of
        beam ->
            % Direct compilation to BEAM
            lx_codegen:compile_direct(AST, #{module_name => ModuleName});
        source ->
            % Generate .erl file
            lx_codegen:generate_erl(AST, #{module_name => ModuleName});
        erl ->
            % Generate .erl file
            lx_codegen:generate_erl(AST, #{module_name => ModuleName});
        ast ->
            % Return AST after macro expansion
            {ok, AST};
        both ->
            % Both BEAM and .erl
            case lx_codegen:compile_direct(AST, #{module_name => ModuleName}) of
                {ok, BeamCode} ->
                    case lx_codegen:generate_erl(AST, #{module_name => ModuleName}) of
                        {ok, ErlCode} ->
                            {ok, ModuleName, BeamCode, #{source => ErlCode}};
                        {error, SourceError} ->
                            {error, SourceError}
                    end;
                {ModuleName, BeamCode, Meta} ->
                    case lx_codegen:generate_erl(AST, #{module_name => ModuleName}) of
                        {ok, ErlCode} ->
                            {ok, ModuleName, BeamCode, Meta#{source => ErlCode}};
                        {error, SourceError} ->
                            {error, SourceError}
                    end;
                {error, BeamError} ->
                    {error, BeamError}
            end;
        _ ->
            % Default to both
            code_generation(AST, Options#{mode => both})
    end.

%% Extract module name from file path
extract_module_name_from_path(FilePath) ->
    % Get the filename without extension
    Filename = filename:basename(FilePath, ".lx"),
    % Convert to atom (Erlang module name)
    list_to_atom(Filename).