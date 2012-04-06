-module(my_lists).
-export([my_lists/0]).

-define(MY_LIST, [1, 2, 3, 4, 5, 6, 7]).

%% find the last box of a list.
lists_last([_|T = [_|_]]) ->
	lists_last(T);
lists_last([H|_]) ->
	H.

%% find the last but one box of a list.
lists_last2([_|T=[_,_|_]]) ->
	lists_last2(T);
lists_last2(L) ->
	L.

%% find the nth element of a list
lists_nth([H|_], N) when N == 1 ->
    H;
lists_nth([_|T], N) ->
    lists_nth(T, N - 1).

%% find the number of elements of a list
lists_size([]) ->
    0;
lists_size([_|T]) ->
    lists_size(T) + 1.

%% reverse a list
lists_reverse([H|[]]) ->
    [H];
lists_reverse([H|T]) ->
    lists_reverse(T) ++ [H].

my_lists() ->
	io:format("last: ~p~n", [lists_last(?MY_LIST)]),
	io:format("last but one: ~p~n", [lists_last2(?MY_LIST)]),
	io:format("5th: ~p~n", [lists_nth(?MY_LIST, 5)]),
	io:format("size: ~p~n", [lists_size(?MY_LIST)]),
	io:format("reverse: ~p~n", [lists_reverse(?MY_LIST)]).

