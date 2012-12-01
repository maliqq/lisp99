-module(logic).
-export([start/0]).

-define(BOOL, [true, false]).

%%
table(F) ->
    [F(A, B) || A <- ?BOOL, B <- ?BOOL].
table2(F) ->
    [F(A, B, C) || A <- ?BOOL, B <- ?BOOL, C <- ?BOOL].

start() ->
    %% (46) truth tables for logical expressions
    table(fun(A, B) -> io:format("~p\t~p\t~p~n", [A, B, A and (A or B)]) end),
    io:format("~n"),
    %% (47) truth tables for logical expressions #2
    table(fun(A, B) -> io:format("~p\t~p\t~p~n", [A, B, A and (A or not B)]) end),
    io:format("~n"),
    %% (48) truth tables for logical expressions #2
    table2(fun(A, B, C) -> io:format("~p\t~p\t~p\t~p~n", [A, B, C, A and (B or C) == A and B or A and C]) end).

