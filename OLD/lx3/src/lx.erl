-module(lx).

-include("lx.hrl").

-export([compile/1, compile/2, parse/1, tokenize/1]).

%% Main compilation pipeline
compile(Source) ->
    compile(Source, #{}).

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

%% Lexical analysis phase
lexical_analysis(Source) ->
    lx_lexer:tokenize(Source).

%% Syntactic analysis phase
syntactic_analysis(Tokens) ->
    lx_parser:parse(Tokens).

%% Macro expansion with type checking
macro_expansion_with_types(AST) ->
    % Start with empty macro environment - no built-in signatures
    MacroEnv = lx_types:new_type_env(),
    case lx_macros:expand_macros_with_types(AST, MacroEnv) of
        {ok, ExpandedAST} ->
            {ok, ExpandedAST};
        {error, Error} ->
            {error, {macro_expansion_error, Error}}
    end.

%% Type analysis phase (inference + meta update)
type_analysis(AST) ->
    case lx_types:infer_types(AST) of
        {ok, TypedAST, Specs} ->
            {ok, TypedAST, Specs};
        {error, Error} ->
            {error, {type_analysis_error, Error}}
    end.

%% Code generation phase
code_generation(AST, Specs, Options) ->
    % For now, just return the typed AST
    % In a full implementation, this would generate Erlang code
    {ok, AST, Specs}.

%% Convenience functions
parse(Source) ->
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            syntactic_analysis(Tokens);
        {error, Error} ->
            {error, Error}
    end.

tokenize(Source) ->
    lexical_analysis(Source).