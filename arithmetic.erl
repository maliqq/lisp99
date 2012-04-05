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

is_prime(N) when N == 1 -> false;

is_prime(N) when N == 2 -> true;

is_prime(N) ->
	test_prime(N, erlang:trunc(math:sqrt(N))).

%% (32) Determine the greatest common divisor of two positive integer numbers
g_common_divisor(N, M) ->
    Rem = N rem M,
    if
        Rem == 0 -> M;
        true -> g_common_divisor(M, Rem)
    end.

%% (33) Determine whether two positive integer numbers are coprime
co_prime(N, M) ->
    case g_common_divisor(N, M) of
        1 -> true;
        _Else -> false
    end.

%% (34) Calculate Euler's totient function phi(m)
totient_phi(_, M) when M == 1 ->
    1;
totient_phi(N, M) ->
    case co_prime(N, M) of
        true -> 1 + totient_phi(N, M - 1);
        false -> totient_phi(N, M - 1)
    end.
totient_phi(1) -> 1;
totient_phi(N) ->
    totient_phi(N, N - 1).

arithmetic() ->
    lists:map(fun(N) ->
        case is_prime(N) of
            true -> io:format("~p is prime~n", [N]);
            _Else -> N
        end
    end, lists:seq(1, 100)),
    io:format("1071, 462 = ~p~n", [g_common_divisor(1071, 462)]),
    io:format("14 coprime 25: ~p, 15 coprime 25: ~p~n", [co_prime(14, 25), co_prime(15, 25)]),
    io:format("phi(10) = ~p~n", [totient_phi(10)]).

