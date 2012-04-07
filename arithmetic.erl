-module(arithmetic).
-export([start/0]).

%% (31) Determine whether a given integer number is prime
test_prime(_, M) when M == 1 -> true;
test_prime(N, M) ->
	case N rem M of
		0 -> false;
		_Else -> test_prime(N, M - 1)
	end.

is_prime(N) when N == 1 -> false;
is_prime(N) when N == 2 -> true;
is_prime(N) -> test_prime(N, erlang:trunc(math:sqrt(N))).

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
phi(_, M) when M == 1 -> 1;
phi(N, M) ->
    case co_prime(N, M) of
        true -> 1 + phi(N, M - 1);
        false -> phi(N, M - 1)
    end.
phi(1) -> 1;
phi(N) -> phi(N, N - 1).

%% (35) Determine the prime factors of a given positive integer
prime_factors(N, M) ->
    case is_prime(M) of
        true when N == M -> [M];
        true when N rem M == 0 -> [M] ++ prime_factors(N div M, 2);
        _Else -> prime_factors(N, M + 1)
    end.

prime_factors(N) -> prime_factors(N, 2).

%% (36) Determine the prime factors of a given positive integer
%% Construct a list containing the prime factors and their multiplicity.
prime_factors2(N, M, Count) ->
    case is_prime(M) of
        true when N == M -> [{M, Count + 1}];
        true when N rem M == 0 -> prime_factors2(N div M, M, Count + 1);
        true when Count > 0 -> [{M, Count}] ++ prime_factors2(N, M + 1, 0); 
        _Else -> prime_factors2(N, M + 1, 0)
    end.

prime_factors2(N) -> prime_factors2(N, 2, 0).

%% (37) calculate Euler's totient function phi(m) (improved).
phi2(N) when is_integer(N) -> phi2([fun(J) ->
        {P, M} = J,
        (P - 1) * math:pow(P, M - 1)
    end(I) || I <- prime_factors2(N)]);
phi2([]) -> 1;
phi2([H|T]) -> H * phi2(T).

%% (38) compare the two methods of calculating Euler's totient function.
compare_phi(N) ->
    {Time1, Result1} = timer:tc(fun phi/1, [N]),
    {Time2, Result2} = timer:tc(fun phi2/1, [N]),
    {Time1, Time2, Result1, Result2}.

%% (39) a list of prime numbers
prime_numbers(X, Y) -> [I || I <- lists:seq(X, Y), is_prime(I)].

start() ->
    lists:map(fun(N) ->
        case is_prime(N) of
            true -> io:format("~p ", [N]);
            _Else -> N
        end
    end, lists:seq(1, 100)),
    io:format("~n1071, 462 = ~p~n", [g_common_divisor(1071, 462)]),
    io:format("14 coprime 25: ~p, 15 coprime 25: ~p~n", [co_prime(14, 25), co_prime(15, 25)]),
    io:format("phi(10) = ~p~n", [phi(10)]),
    io:format("prime factors of 315: ~p~n", [prime_factors(315)]),
    io:format("prime factors of 315: ~p~n", [prime_factors2(315)]),
    io:format("phi(10) = ~p~n", [phi2(10)]),
    io:format("compare phi(10090) = ~p~n", [compare_phi(10090)]),
    io:format("prime numbers from 100 to 1000: ~p~n", [prime_numbers(100, 1000)]).

