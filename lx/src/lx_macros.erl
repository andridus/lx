-module(lx_macros).
-export([register_macro/4, expand_macros/1, get_macro/2]).

% Registry to store defined macros
% Format: {macro_name, {arity, precedence, associativity, modifiers}, macro_body}
-record(macro_registry, {
    macros = #{}
}).

% Initialize macro registry
init() ->
    #macro_registry{}.

% Register a new macro
register_macro(Name, Metadata, Body, Registry) ->
    Registry#macro_registry{macros = maps:put(Name, {Metadata, Body}, Registry#macro_registry.macros)}.

% Get a macro by name
get_macro(Name, Registry) ->
    maps:get(Name, Registry#macro_registry.macros, not_found).

% Expand macros in AST
expand_macros(AST) ->
    Registry = init(),
    expand_macros(AST, Registry).

expand_macros([], _Registry) ->
    [];
expand_macros([Node | Rest], Registry) ->
    [expand_node(Node, Registry) | expand_macros(Rest, Registry)];
expand_macros(Node, Registry) ->
    expand_node(Node, Registry).

% Expand a single AST node
expand_node({macro_def, Line, Name, Params, Body}, Registry) ->
    % Register the macro
    Metadata = {length(Params), 1, any, [{args, Params}]},
    _NewRegistry = register_macro(Name, Metadata, Body, Registry),
    % Return the macro definition as is for now
    {macro_def, Line, Name, Params, Body};
expand_node({macro_def_infix, Line, Name, Params, Body}, Registry) ->
    % Register the infix macro
    Metadata = {length(Params), 1, any, [infix]},
    _NewRegistry = register_macro(Name, Metadata, Body, Registry),
    % Return the macro definition as is for now
    {macro_def_infix, Line, Name, Params, Body};
expand_node({do_block, Line, Expressions}, Registry) ->
    % Expand expressions in do block
    ExpandedExpressions = expand_macros(Expressions, Registry),
    {do_block, Line, ExpandedExpressions};
expand_node(Node, _Registry) ->
    % For now, return the node as is
    % In a full implementation, this would check if the node is a macro call
    Node.