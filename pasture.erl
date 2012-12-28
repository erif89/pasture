-module(pasture).

% Advanced Function Programming, Project - Pasture in Erlang
-author('Emil Wall <emwa3503@student.uu.se>').
-author('Carl Carenvall <caca7037@student.uu.se>').

-include_lib("eunit/include/eunit.hrl").

-export([]).

% Returns the list of prime numbers less than or equal to N
% Algorithm: Use a list comprehension to filter out non-primes <= N
% Built-in function lists:seq is used to generate the range 2..N
% The filtering is done by simple guards and the help function is_prime (below)
-spec primes_up_to(integer()) -> [integer()].
primes_up_to(N) when N < 2 ->
    [];
primes_up_to(N) ->
    [X || X <- lists:seq(2, N),
        (X < 4) or
        (X rem 2 /= 0) and is_prime(X, 3, math:sqrt(X))].

% Returns true if no number in [K, K+2, .., S] divides X; false otherwise
% The first clause appears redundant when used by primes_up_to, 
% but is necessary to use it as a stand-alone function (see factorize).
is_prime(X, _, _) when X < 4 ->
	true;
is_prime(X, K, S) when K >= S ->
    X rem K /= 0;
is_prime(X, K, S) when X rem K > 0 ->
    is_prime(X, K+2, S);
is_prime(_, _, _) ->
    false.


primes_up_to_test_() ->
    [?_assert(primes_up_to(-7) =:= []),
     ?_assert(primes_up_to(-1) =:= []),
     ?_assert(primes_up_to(0) =:= []),
     ?_assert(primes_up_to(2) =:= [2]),
     ?_assert(primes_up_to(3) =:= [2, 3]),
     ?_assert(primes_up_to(5) =:= [2,3,5]),
     ?_assert(primes_up_to(10) =:= [2,3,5,7]),
     ?_assert(primes_up_to(11) =:= [2,3,5,7,11]),
     ?_assert(primes_up_to(20) =:= [2,3,5,7,11,13,17,19]),
     ?_assert(primes_up_to(30) =:= [2,3,5,7,11,13,17,19,23,29]),
     ?_assert(primes_up_to(1000) =:= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31,
        37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
        109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181,
        191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263,
        269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
        353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433,
        439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521,
        523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613,
        617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701,
        709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809,
        811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887,
        907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997])
    ].
