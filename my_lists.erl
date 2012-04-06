-module(my_lists).
-export([my_lists/0]).

-define(MY_LIST, [1, 2, 3, 4, 5, 6, 7]).

%% find the last box of a list.
last([_|T=[_|_]]) -> last(T);
last([H|_]) -> H.

%% find the last but one box of a list.
last2([_|T=[_,_|_]]) -> last2(T);
last2(L) -> L.

%% find the nth element of a list
nth([H|_], N) when N == 1 -> H;
nth([_|T], N) -> nth(T, N - 1).

%% find the number of elements of a list
lsize([]) -> 0;
lsize([_|T]) -> lsize(T) + 1.

%% reverse a list
lreverse([H|[]]) ->
    [H];
lreverse([H|T]) ->
    lreverse(T) ++ [H].

%% find out whether a list is a palindrome.
is_palindrome([]) -> true;
is_palindrome([_|_=[]]) -> true;
is_palindrome(L) -> L == lreverse(L).

%% flatten a nested list structure
flatten([]) -> [];
flatten([H|T]) when is_list(H) -> flatten(H) ++ flatten(T);
flatten([H|T]) -> [H] ++ flatten(T).

%% eliminate consecutive duplicates of list elements
scan_while([H|T], H2) when H == H2 -> scan_while(T, H2);
scan_while(_=[], _) -> [];
scan_while([H|T], H2) -> uniq1(T).

uniq1([H|_=[H2|T]]) when H == H2 -> [H] ++ scan_while(T, H);
uniq1([H|T]) -> [H] ++ uniq1(T).

my_lists() ->
	io:format("last: ~p~n", [last(?MY_LIST)]),
	io:format("last but one: ~p~n", [last2(?MY_LIST)]),
	io:format("5th: ~p~n", [nth(?MY_LIST, 5)]),
	io:format("size: ~p~n", [lsize(?MY_LIST)]),
	io:format("reverse: ~p~n", [lreverse(?MY_LIST)]),
	io:format("palindrome? ~p~n", [is_palindrome([1, 2, 3, 2, 1, 2])]),
	io:format("flatten ~p~n", [flatten([1, [2, 3], [4, 5, 6], [7], 8, 9])]),
	io:format("uniq1 ~p~n", [uniq1([1, 1, 1, 1, 1, 2, 3, 1, 1, 2, 2, 2, 4, 5, 6, 7, 7])]).

