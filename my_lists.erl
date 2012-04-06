-module(my_lists).
-export([my_lists/0]).

-define(MY_LIST, [1, 2, 3, 4, 5, 6, 7]).
-define(DUP_LIST, [1, 1, 1, 1, 1, 2, 3, 1, 1, 2, 2, 2, 4, 5, 6, 7, 7, 7]).

%% (1) find the last box of a list.
last([_|T=[_|_]]) -> last(T);
last([H|_]) -> H.

%% (2) find the last but one box of a list.
last2([_|T=[_,_|_]]) -> last2(T);
last2(L) -> L.

%% (3) find the nth element of a list
nth([H|_], N) when N == 1 -> H;
nth([_|T], N) -> nth(T, N - 1).

%% (4) find the number of elements of a list
lsize([]) -> 0;
lsize([_|T]) -> lsize(T) + 1.

%% (5) reverse a list
lreverse([H|[]]) -> [H];
lreverse([H|T]) -> lreverse(T) ++ [H].

%% (6) find out whether a list is a palindrome.
is_palindrome([]) -> true;
is_palindrome([_|_=[]]) -> true;
is_palindrome(L) -> L == lreverse(L).

%% (7) flatten a nested list structure
flatten([]) -> [];
flatten([H|T]) when is_list(H) -> flatten(H) ++ flatten(T);
flatten([H|T]) -> [H] ++ flatten(T).

%% (8) eliminate consecutive duplicates of list elements
elim_dup([H|_=[Dup|T]]) when H == Dup -> elim_dup([H] ++ T);
elim_dup([]) -> [];
elim_dup(Dup=[_]) -> Dup; %% last
elim_dup([H|T]) -> [H] ++ elim_dup(T).

%% (9) pack consecutive duplicates of list elements into sublists
pack_dup([H|T=[Dup|_]], Buf) when H == Dup -> pack_dup(T, Buf ++ [H]);
pack_dup([H|T], Buf) -> [Buf ++ [H]] ++ pack_dup(T).

pack_dup([H|T=[Dup|_]]) when H == Dup -> pack_dup(T, [H]);
pack_dup([]) -> [];
pack_dup([H|T]) -> [[H]] ++ pack_dup(T).

%% (10) run-length encoding of a list
count_dup2(L) -> [X|_] = L, {erlang:length(L), X}.
count_dup(L) -> [count_dup2(X) || X <- pack_dup(L)].

%% (11) modified run-length encoding
count_dup3([X|_=[]]) -> X;
count_dup3(L) -> [X|_] = L, {erlang:length(L), X}.
count2_dup(L) when is_list(L) -> [count_dup3(X) || X <- pack_dup(L)].

%% (12) decode a run-length encoded list
decode_dup(X, N) when N == 1 -> [X];
decode_dup(X, N) when N > 1 -> [X] ++ decode_dup(X, N - 1).

decode_dup(L) when is_tuple(L) -> {N, X} = L, decode_dup(X, N);
decode_dup(L=[H|T]) when is_list(L) -> decode_dup(H) ++ decode_dup(T);
decode_dup([]) -> [];
decode_dup(L) -> [L].

%% (13) run-length encoding of a list (direct solution)
count3_dup([H|T=[Dup|_]], Count) when H == Dup -> count3_dup(T, Count + 1);
count3_dup([H|T], Count) -> [{Count + 1, H}] ++ count3_dup(T).

count3_dup([H|T=[Dup|_]]) when H == Dup -> count3_dup(T, 1);
count3_dup([]) -> [];
count3_dup([H|T]) -> [H] ++ count3_dup(T).

%% (14) duplicate the elements of a list
dupl([]) -> [];
dupl([X|T]) -> [X, X] ++ dupl(T).

%% (15) replicate the elements of a list a given number of times
repl(X, N) when  N == 1 -> [X];
repl([], _) -> [];
repl(L=[H|T], N) when is_list(L) -> repl(H, N) ++ repl(T, N);
repl(X, N) -> [X] ++ repl(X, N - 1).

my_lists() ->
	io:format("last: ~p~n", [last(?MY_LIST)]),
	io:format("last but one: ~p~n", [last2(?MY_LIST)]),
	io:format("5th: ~p~n", [nth(?MY_LIST, 5)]),
	io:format("size: ~p~n", [lsize(?MY_LIST)]),
	io:format("reverse: ~p~n", [lreverse(?MY_LIST)]),
	io:format("palindrome? ~p~n", [is_palindrome([1, 2, 3, 2, 1, 2])]),
	io:format("flatten ~p~n", [flatten([1, [2, 3], [4, 5, 6], [7], 8, 9])]),
	io:format("elim_dup ~p~n", [elim_dup(?DUP_LIST)]),
	io:format("pack_dup ~p~n", [pack_dup(?DUP_LIST)]),
	io:format("count_dup ~p~n", [count_dup(?DUP_LIST)]),
	io:format("count2_dup ~p~n", [count2_dup(?DUP_LIST)]),
	io:format("decode_dup ~p~n", [decode_dup([{3, 5}, 2, 3, 4, {2, 8}, {4, 11}, 0])]),
	io:format("count3_dup ~p~n", [count3_dup(?DUP_LIST)]),
	io:format("dup ~p~n", [dupl([1, 2, 3, 4, 5, 6])]),
	io:format("repl ~p~n", [repl([1, 2, 3, 4, 5, 6], 3)]).

