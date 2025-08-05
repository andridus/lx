-module(lx_compiler).
-export([compile/1, compile/2, compile_file/1, compile_file/2, code_generation/2]).

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

compile(Source) ->
    compile(Source, #{}).

compile(Source, Options) ->
    % Phase 1: Lexical analysis
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            % Phase 2: Syntactic analysis
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    % Phase 3: Macro expansion
                    case lx_macros:expand_macros(AST) of
                        {error, {undefined_macro, Line, Name, Arg}} ->
                            ErrorMsg = lists:flatten(io_lib:format("Undefined macro '~p' called with argument ~p", [Name, Arg])),
                            {error, {macro_error, Line, lx_macros, ErrorMsg}};
                        {error, Error} ->
                            {error, Error};
                        ExpandedAST ->
                            % Phase 4: Code generation
                            code_generation(ExpandedAST, Options)
                    end;
                {error, {Line, Module, Message}} ->
                    {error, {parse_error, Line, Module, Message}}
            end;
        {error, {Line, Module, Message}, _} ->
            {error, {lexer_error, Line, Module, Message}}
    end.

%% Lexical analysis phase
lexical_analysis(Source) ->
    case lx_lexer:string(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, Error, _EndLine} ->
            {error, Error}
    end.

%% Syntactic analysis phase
syntactic_analysis(Tokens) ->
    lx_parser:parse(Tokens).

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