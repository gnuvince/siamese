-module(siamese).

-export([
         new/0,
         to_list/1,
         from_list/1,
         open_scope/1,
         close_scope/1,
         add/3,
         remove/2,
         find/2,
         get/2,
         get/3,
         size/1
]).


%% A symbol table is represented as a stack of maps.
%% The stack allows having multiple maps representing
%% different levels of scope nesting.


%% Return an empty symbol table.
new() ->
    [maps:new()].


%% Create a symbol table from a list of {Symbol, Value} tuples.
to_list(Symtable) ->
    lists:flatten([maps:to_list(Map) || Map <- Symtable]).


%% Convert the symbol table to a list of {Symbol, Value} tuples.
from_list(List) ->
    lists:foldl(fun ({K, V}, Symtable) -> add(K, V, Symtable) end,
                new(),
                List).


%% Open a new scope in a symbol table by consing a new
%% empty map at the beginning of the stack.  This allows
%% shadowing of identifiers.
open_scope(Symtable) ->
    [maps:new() | Symtable].


%% Close the top-most scope by popping it off the stack.
close_scope([_ | Symtable]) ->
    Symtable.


%% Try to find a symbol in the symbol table starting from the
%% inner-most scope to the outer-most.  Return the tuple
%% {ok, Value} if Symbol is found, the atom symbol_not_found
%% otherwise.
find(_, []) ->
    symbol_not_found;
find(Symbol, [Map | Rest]) ->
    case maps:find(Symbol, Map) of
        {ok, Value} -> {ok, Value};
        error -> find(Symbol, Rest)
    end.


%% Return the value associated with Symbol; if Symbol is
%% not found in the symbol table, raise an error.
get(Symbol, Symtable) ->
    case find(Symbol, Symtable) of
        {ok, Value} -> Value;
        symbol_not_found -> error({badkey, Symbol})
    end.


%% Return the value associated with Symbol; if Symbol is
%% not found in the symbol table, return Default.
get(Symbol, Symtable, Default) ->
    case find(Symbol, Symtable) of
        {ok, Value} -> Value;
        symbol_not_found -> Default
    end.


%% Try to add a new Symbol/Value pair in the current scope;
%% return the updated symbol table, or the atom
%% key_already_exists if Symbol already exists in the current
%% scope.
%% Return the atom invalid_symbol_table if the symbol table
%% is in an invalid state, i.e., an empty list containing no
%% scopes.
add(_Symbol, _Value, []) ->
    invalid_symbol_table;
add(Symbol, Value, [Map | Rest]) ->
    case maps:is_key(Symbol, Map) of
        true -> key_already_exists;
        false -> [maps:put(Symbol, Value, Map) | Rest]
    end.


%% Remove a Symbol/Value pair from the current scope.
%% Return the atom invalid_symbol_table if the symbol table
%% is in an invalid state, i.e., an empty list containing no
%% scopes.
remove(_Symbol, []) ->
    invalid_symbol_table;
remove(Symbol, [Map | Rest]) ->
    [maps:remove(Symbol, Map) | Rest].


size(Symtable) ->
    lists:sum([maps:size(M) || M <- Symtable]).
