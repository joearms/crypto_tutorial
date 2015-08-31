-module(ez_crypt_primes).

%% -compile(export_all).
-export([make_k_bit_prime/1, is_prime/1, test/0]).

-import(crypto, [strong_rand_bytes/1, rand_uniform/2]).
-import(ez_crypt_math, [k_bit_random_integer/1, bsize/1]).

test() ->
    test_small_primes().

make_k_bit_prime(Bits) when Bits > 7, Bits < 17 ->
    %% us a different method
    %% generate random number sin 1..2^N
    %% and test
    small_random_prime(Bits);
make_k_bit_prime(Bits) when Bits < 8 ->
    exit({?MODULE,make_prime,too_few_bits,Bits});
make_k_bit_prime(Bits) ->
    %% DLen = length in bits
    N = k_bit_random_integer(Bits),
    %% N is an odd number
    {Prime,_K} = long_loop(N, 0),
    case bsize(Prime) of
	Bits ->
	    Prime;
	N1 ->
	    io:format("** something is wrong wanted ~p bit prime~n"
		      " generated ~p bit prime~ntrying agian~n",
		      [Bits,N1]),
	    make_k_bit_prime(Bits)
    end.

long_loop(P, K) ->
    case is_prime(P) of
	true ->
	    {P,K};
	false ->
	    long_loop(P+2, K+1)
    end.


test_small_primes() ->
    L = lists:seq(3,2000),
    S = small_primes(),
    lists:foreach(fun(I) ->
			  %% io:format("I=~p is_prime:~p~n",[I,is_prime(I)]),
			  case lists:member(I, S) of
			      true ->
				  true = is_prime(I);
			      false ->
				  false = is_prime(I)
			  end
		  end, L).

%% START:is_prime
is_prime(0) -> false;
is_prime(1) -> false;			  
is_prime(X) ->
    case is_multiple_of_small_prime(X) of
	{yes, X}  -> true;
	{yes,_}   -> false;
	no        -> ez_crypt_miller_rabin:is_probably_prime(X)
    end.
    
is_multiple_of_small_prime(N) ->
    is_multiple_of_small_prime(N, small_primes()).

is_multiple_of_small_prime(N, [H|T]) ->
    case N rem H of
	0 -> {yes, H};
	_ -> is_multiple_of_small_prime(N, T)
    end;
is_multiple_of_small_prime(_, []) ->
    no.

%% END:is_prime   

small_primes() ->
    %% < 2000
    [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,
     71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,
     149,151,157,163,167,173,179,181,191,193,197,
     199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,
     313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,
     433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,
     563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,
     673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,
     811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,
     941,947,953,967,971,977,983,991,997,1009,1013,1019,1021,1031,1033,1039,1049,
     1051,1061,1063,1069,1087,1091,1093,1097,1103,1109,1117,1123,1129,1151,1153,
     1163,1171,1181,1187,1193,1201,1213,1217,1223,1229,1231,1237,1249,1259,1277,
     1279,1283,1289,1291,1297,1301,1303,1307,1319,1321,1327,1361,1367,1373,1381,
     1399,1409,1423,1427,1429,1433,1439,1447,1451,1453,1459,1471,1481,1483,1487,
     1489,1493,1499,1511,1523,1531,1543,1549,1553,1559,1567,1571,1579,1583,1597,
     1601,1607,1609,1613,1619,1621,1627,1637,1657,1663,1667,1669,1693,1697,1699,
     1709,1721,1723,1733,1741,1747,1753,1759,1777,1783,1787,1789,1801,1811,1823,
     1831,1847,1861,1867,1871,1873,1877,1879,1889,1901,1907,1913,1931,1933,1949,
     1951,1973,1979,1987,1993,1997,1999].


%% Small numbers
%% K = 1 bit 0 or 1 - invlaid
%% K = 2 0..3   the prime is 2 or 3
%% K = 3 0..7   2 3 5
%% K = 4 0..15  2 3 5 7 11
%% K = 5 0..31  2 3 5 7 11 13 17 19 23 29

small_random_prime(K) ->
    N = k_bit_random_integer(K),
    io:format("tryimg:~p~n",[N]),
    case is_prime(N) of
	true  -> N;
	false -> small_random_prime(K)
    end.

    
