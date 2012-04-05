-module(arithmetic).
-export([arithmetic/0]).

%% (31) Determine whether a given integer number is prime
is_divisable(N, M) ->
	if
		N rem M == 0 -> true;
		true -> false
	end.

test_prime(_, M) when M == 1 ->
    true;

test_prime(N, M) ->
	case is_divisable(N, M) of
	    true -> false;
		_Else -> test_prime(N, M - 1)
	end.

is_prime(N) when N == 1 ->
	false;

is_prime(N) when N == 2 ->
	true;

is_prime(N) ->
	test_prime(N, erlang:trunc(math:sqrt(N))).

arithmetic() ->
	is_prime(4).

