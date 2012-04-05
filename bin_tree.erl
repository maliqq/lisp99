-module(bin_tree).
-export([bin_tree/0]).

%% Check whether a given term represents a binary tree
is_tree(_) ->
    false.

bin_tree() ->
    is_tree({a, {b, none, none}, none}), %% true
    is_tree({a, {b, none, none}}). %% false

