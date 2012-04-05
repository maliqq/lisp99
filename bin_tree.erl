-module(bin_tree).
-export([bin_tree/0]).

%% Check whether a given term represents a binary tree
is_tree(Tree) when is_tuple(Tree) ->
    case tuple_size(Tree) of
        3 ->
            Left = element(2, Tree), Right = element(3, Tree), is_tree(Left) and is_tree(Right);
        _Else -> false
    end;
is_tree(_) ->
    true.

bin_tree() ->
    io:format("~p~n", [is_tree({a, {b, none, none}, none})]),
    io:format("~p~n", [is_tree({a, {b, none, none}})]).

