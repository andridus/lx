-module(lx2).

-include("lx2.hrl").

-export([compile/1, compile/2, parse/1]).

%% Main compilation function
compile(Source) ->
    compile(Source, #{}).

%% Parse LX source code (for testing)
parse(Source) ->
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, SyntaxError} ->
                    {error, SyntaxError}
            end;
        {error, LexicalError} ->
            {error, LexicalError}
    end.

%% Compile LX source code with Elixir-style AST and macro system
compile(Source, Options) ->
    % Phase 1: Lexical analysis
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            % Phase 2: Syntactic analysis
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    % Phase 3: Macro expansion with type checking
                    case macro_expansion_with_types(AST) of
                        {ok, ExpandedAST} ->
                            % Phase 4: Type inference and meta update
                            case type_analysis(ExpandedAST) of
                                {ok, TypedAST, Specs} ->
                                    % Phase 5: Code generation
                                    code_generation(TypedAST, Specs, Options);
                                {error, TypeErrors} ->
                                    {error, TypeErrors}
                            end;
                        {error, MacroErrors} ->
                            {error, MacroErrors}
                    end;
                {error, SyntaxError} ->
                    {error, SyntaxError}
            end;
        {error, LexicalError} ->
            {error, LexicalError}
    end.

%% Lexical analysis
lexical_analysis(Source) ->
    case lx2_lexer:string(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, {Line, leex, {illegal, Char}}, _EndLine} ->
            {error, {lexical_error, Line, "illegal character: " ++ [Char]}};
        {error, {Line, leex, {user, Error}}, _EndLine} ->
            {error, {lexical_error, Line, Error}};
        {error, {Line, _Module, Error}, _EndLine} ->
            {error, {lexical_error, Line, "lexical error: " ++ lists:flatten(io_lib:format("~p", [Error]))}}
    end.

%% Syntactic analysis
syntactic_analysis(Tokens) ->
    case lx2_parser:parse(Tokens) of
        {ok, AST} ->
            {ok, AST};
        {error, {syntax_error, Message}} ->
            {error, {syntax_error, 1, Message}};
        {error, {Line, yecc, {syntax_error, Message}}} ->
            {error, {syntax_error, Line, Message}};
        {error, {Line, yecc, {user, Error}}} ->
            {error, {syntax_error, Line, Error}};
        {error, {Line, _Module, Error}} ->
            {error, {syntax_error, Line, Error}}
    end.

%% Macro expansion with type checking
macro_expansion_with_types(AST) ->
    % Start with empty macro environment - no built-in signatures
    MacroEnv = lx2_macros:new_macro_env(),
    case lx2_macros:expand_macros_with_types(AST, MacroEnv) of
        {ok, ExpandedAST} ->
            {ok, ExpandedAST};
        {error, Error} ->
            {error, {macro_expansion_error, Error}}
    end.

%% Type analysis phase (inference + meta update)
type_analysis(AST) ->
    case lx2_types:infer_types(AST) of
        {ok, TypedAST, Specs} ->
            {ok, TypedAST, Specs};
        {error, Error} ->
            {error, {type_analysis_error, Error}}
    end.

%% Code generation
code_generation(AST, Specs, Options) ->
    ModuleName = maps:get(module_name, Options, unknown),
    case maps:get(mode, Options, both) of
        beam ->
            lx2_codegen:compile_direct(AST, ModuleName);
        source ->
            lx2_codegen:generate_erl_with_specs(AST, ModuleName, Specs);
        erl ->
            lx2_codegen:generate_erl_with_specs(AST, ModuleName, Specs);
        ast ->
           {ok, AST};
        both ->
            case lx2_codegen:compile_direct(AST, ModuleName) of
                {ModuleName, BeamCode, Meta} ->
                    case lx2_codegen:generate_erl_with_specs(AST, ModuleName, Specs) of
                        {ok, ErlCode} ->
                            {ok, ModuleName, BeamCode, Meta#{source => ErlCode}};
                        {error, SourceError} ->
                            {error, SourceError}
                    end;
                {error, BeamError} ->
                    {error, BeamError}
            end
    end.