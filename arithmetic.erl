-module(arithmetic).
-export([arithmetic/0]).

%% (31) Determine whether a given integer number is prime
is_divisable(N, M) ->
	(N rem M == 0).

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

%% (35) Determine the prime factors of a given positive integer
prime_factors(N, M) ->
    case is_prime(M) of
        true when N == M -> [M];
        true when N rem M == 0 -> [M] ++ prime_factors(N div M, 2);
        _Else -> prime_factors(N, M + 1)
    end.

prime_factors(N) ->
    prime_factors(N, 2).

%% (36) Determine the prime factors of a given positive integer
%% Construct a list containing the prime factors and their multiplicity.
prime_factors2(N, M, Count) ->
    case is_prime(M) of
        true when N == M -> [{M, Count + 1}];
        true when N rem M == 0 -> prime_factors2(N div M, M, Count + 1);
        true when Count > 0 -> [{M, Count}] ++ prime_factors2(N, M + 1, 0); 
        _Else -> prime_factors2(N, M + 1, 0)
    end.

prime_factors2(N) ->
    prime_factors2(N, 2, 0).

arithmetic() ->
    lists:map(fun(N) ->
        case is_prime(N) of
            true -> io:format("~p is prime~n", [N]);
            _Else -> N
        end
    end, lists:seq(1, 100)),
    io:format("1071, 462 = ~p~n", [g_common_divisor(1071, 462)]),
    io:format("14 coprime 25: ~p, 15 coprime 25: ~p~n", [co_prime(14, 25), co_prime(15, 25)]),
    io:format("phi(10) = ~p~n", [totient_phi(10)]),
    io:format("prime factors of 315: ~p~n", [prime_factors(315)]),
    io:format("prime factors of 315: ~p~n", [prime_factors2(315)]).

