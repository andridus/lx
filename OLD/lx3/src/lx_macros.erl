-module(lx_macros).

-include("lx.hrl").

-export([expand_macros_with_types/2, register_macro_signature/4, register_infix_macro_signature/4]).

%% Expand macros with type checking
expand_macros_with_types(AST, TypeEnv) ->
    case expand_macros_recursive(AST, TypeEnv) of
        {ok, ExpandedAST} ->
            {ok, ExpandedAST};
        {error, Error} ->
            {error, {macro_expansion_error, Error}}
    end.

%% Expand macros recursively
expand_macros_recursive([], _TypeEnv) ->
    {ok, []};
expand_macros_recursive([Node | Rest], TypeEnv) ->
    case expand_node_macros(Node, TypeEnv) of
        {ok, ExpandedNode} ->
            case expand_macros_recursive(Rest, TypeEnv) of
                {ok, ExpandedRest} ->
                    {ok, [ExpandedNode | ExpandedRest]};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
expand_macros_recursive(SingleNode, TypeEnv) ->
    % Handle single node (from parser minimal)
    expand_node_macros(SingleNode, TypeEnv).

%% Expand macros in a single node
expand_node_macros({Type, Meta, Args}, TypeEnv) ->
    case Type of
        defmacro ->
            % Register macro definition
            register_macro_from_ast({Type, Meta, Args}, TypeEnv);
        macro_call ->
            % Expand macro call
            expand_macro_call({Type, Meta, Args}, TypeEnv);
        _ ->
            % Recursively expand arguments if they are lists
            case expand_args_macros(Args, TypeEnv) of
                {ok, ExpandedArgs} ->
                    {ok, {Type, Meta, ExpandedArgs}};
                {error, Error} ->
                    {error, Error}
            end
    end;
expand_node_macros({Type, Line, Col, Value}, TypeEnv) ->
    % Handle parser minimal structure {Type, Line, Col, Value}
    {ok, {Type, Line, Col, Value}};
expand_node_macros(Other, TypeEnv) ->
    % Handle non-AST nodes (atoms, integers, etc.)
    {ok, Other}.

%% Expand macros in arguments
expand_args_macros(Args, TypeEnv) when is_list(Args) ->
    expand_macros_recursive(Args, TypeEnv);
expand_args_macros(Args, TypeEnv) ->
    case expand_node_macros(Args, TypeEnv) of
        {ok, ExpandedArg} ->
            {ok, ExpandedArg};
        {error, Error} ->
            {error, Error}
    end.

%% Register macro from AST definition
register_macro_from_ast({defmacro, Meta, [Name, Args, Body]}, TypeEnv) ->
    % Extract macro signature from typed arguments
    {TypedArgs, ArgTypes} = extract_macro_signature(Args),

    % Register macro signature
    NewTypeEnv = register_macro_signature(Name, ArgTypes, any, TypeEnv),

    % Return the macro definition unchanged
    {ok, {defmacro, Meta, [Name, TypedArgs, Body]}};

%% Register infix macro from AST definition
register_macro_from_ast({defmacro_infix, Meta, [Op, Args, Body]}, TypeEnv) ->
    % Extract macro signature from typed arguments
    {TypedArgs, ArgTypes} = extract_macro_signature(Args),

    % Register infix macro signature
    NewTypeEnv = register_infix_macro_signature(Op, ArgTypes, any, TypeEnv),

    % Return the infix macro definition unchanged
    {ok, {defmacro_infix, Meta, [Op, TypedArgs, Body]}}.

%% Extract macro signature from arguments
extract_macro_signature(Args) ->
    extract_macro_signature(Args, [], []).

extract_macro_signature([], AccArgs, AccTypes) ->
    {lists:reverse(AccArgs), lists:reverse(AccTypes)};
extract_macro_signature([{typed_ident, Meta, [Name, TypeExpr]} | Rest], AccArgs, AccTypes) ->
    Type = extract_type_from_expr(TypeExpr),
    extract_macro_signature(Rest,
                          [{ident, Meta#{type => Type}, Name} | AccArgs],
                          [{Name, Type} | AccTypes]);
extract_macro_signature([{ident, Meta, Name} | Rest], AccArgs, AccTypes) ->
    extract_macro_signature(Rest,
                          [{ident, Meta#{type => any}, Name} | AccArgs],
                          [{Name, any} | AccTypes]);
extract_macro_signature([{'__block__', _Meta, BlockArgs} | Rest], AccArgs, AccTypes) ->
    %% Handle block arguments (empty for now)
    extract_macro_signature(Rest, AccArgs, AccTypes);
extract_macro_signature([Other | Rest], AccArgs, AccTypes) ->
    %% Handle other argument types
    extract_macro_signature(Rest, AccArgs, AccTypes);
extract_macro_signature({'__block__', _Meta, BlockArgs}, AccArgs, AccTypes) ->
    %% Handle single block argument
    {lists:reverse(AccArgs), lists:reverse(AccTypes)};
extract_macro_signature(Other, AccArgs, AccTypes) ->
    %% Handle other argument types
    {lists:reverse(AccArgs), lists:reverse(AccTypes)}.

%% Extract type from type expression
extract_type_from_expr({type_basic, _Meta, Type}) ->
    Type;
extract_type_from_expr({type_fun, _Meta, [ArgTypes, ReturnType]}) ->
    {'fun', [extract_type_from_expr(T) || T <- ArgTypes], extract_type_from_expr(ReturnType)};
extract_type_from_expr({type_list, _Meta, ElementType}) ->
    {list, extract_type_from_expr(ElementType)};
extract_type_from_expr({type_tuple, _Meta, ElementTypes}) ->
    {tuple, [extract_type_from_expr(T) || T <- ElementTypes]};
extract_type_from_expr({type_var, _Meta, VarName}) ->
    {type_var, VarName};
extract_type_from_expr(_) ->
    any.

%% Expand macro call
expand_macro_call({macro_call, Meta, [MacroName, Args]}, TypeEnv) ->
    case get_macro_definition(MacroName, length(Args), TypeEnv) of
        {ok, {_Name, TypedArgs, Body}} ->
            % Create substitution from arguments
            Substitution = create_argument_substitution(TypedArgs, Args),

            % Apply substitution to macro body
            ExpandedBody = apply_macro_substitution(Body, Substitution),

            % Return expanded body
            {ok, ExpandedBody};
        {error, Error} ->
            {error, {macro_not_found, MacroName, Error}}
    end.

%% Get macro definition from environment
get_macro_definition(MacroName, Arity, #{macros := Macros}) ->
    case maps:find(MacroName, Macros) of
        {ok, Signatures} ->
            case find_signature_by_arity(Signatures, Arity) of
                {ok, Signature} -> {ok, Signature};
                error -> {error, {arity_mismatch, MacroName, Arity}}
            end;
        error ->
            {error, {macro_not_found, MacroName}}
    end.

%% Find signature by arity
find_signature_by_arity([{Name, Args, ReturnType} | _], Arity) when length(Args) =:= Arity ->
    {ok, {Name, Args, ReturnType}};
find_signature_by_arity([_ | Rest], Arity) ->
    find_signature_by_arity(Rest, Arity);
find_signature_by_arity([], _Arity) ->
    error.

%% Create argument substitution
create_argument_substitution(TypedArgs, ActualArgs) ->
    create_argument_substitution(TypedArgs, ActualArgs, #{}).

create_argument_substitution([], [], Substitution) ->
    Substitution;
create_argument_substitution([{Name, _Type} | TypedRest], [ActualArg | ActualRest], Substitution) ->
    NewSubstitution = Substitution#{Name => ActualArg},
    create_argument_substitution(TypedRest, ActualRest, NewSubstitution);
create_argument_substitution(_, _, _) ->
    #{}.

%% Apply macro substitution to body
apply_macro_substitution(Body, Substitution) ->
    case Body of
        [SingleExpr] ->
            apply_substitution_to_node(SingleExpr, Substitution);
        ExprList when is_list(ExprList) ->
            [apply_substitution_to_node(Expr, Substitution) || Expr <- ExprList];
        _ ->
            Body
    end.

%% Apply substitution to a single node
apply_substitution_to_node({Type, Meta, Args}, Substitution) ->
    case Type of
        ident ->
            case maps:find(Args, Substitution) of
                {ok, Value} ->
                    Value;
                error ->
                    {Type, Meta, Args}
            end;
        _ ->
            case expand_args_macros(Args, #{}) of
                {ok, ExpandedArgs} ->
                    {Type, Meta, ExpandedArgs};
                {error, _} ->
                    {Type, Meta, Args}
            end
    end.

%% Register macro signature when macro is defined
register_macro_signature(MacroName, Args, ReturnType, TypeEnv) ->
    #{macros := Macros} = TypeEnv,
    NewMacros = maps:update_with(MacroName,
        fun(Signatures) -> [{MacroName, Args, ReturnType} | Signatures] end,
        [{MacroName, Args, ReturnType}],
        Macros),
    TypeEnv#{macros := NewMacros}.

%% Register infix macro signature when macro is defined
register_infix_macro_signature(Op, Args, ReturnType, TypeEnv) ->
    InfixMacros = maps:get(infix_macros, TypeEnv, #{}),
    NewInfixMacros = maps:update_with(Op,
        fun(Signatures) -> [{Op, Args, ReturnType} | Signatures] end,
        [{Op, Args, ReturnType}],
        InfixMacros),
    TypeEnv#{infix_macros := NewInfixMacros}.