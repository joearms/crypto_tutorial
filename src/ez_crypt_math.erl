-module(ez_crypt_math).

%% -compile(export_all).

-export([bsize/1,
	 dsize/1,
	 gcd/2,
	 k_bit_random_integer/1,
	 make_rsa_keypair/1,
	 mod_pow/3,
	 str2int/1,
	 int2str/1,
	 inv/2,
	 is_even/1,
	 random_integer/1,
	 test/0]).

-import(crypto, [strong_rand_bytes/1, rand_uniform/2]).

-spec mod_pow(N::integer(), P::integer(), M::integer()) -> R::integer().

%% random_integer(Len) -> Int.


test() ->
    t1(<<"123">>),
    t1(<<0,1,2,3>>),
    t1(<<0,0,0,1,2,3>>),
    t1(<<>>),
    67 = inv(28, 75),
    6597 = inv(3533, 11200),
    3533 = inv(6597, 11200),
    5761 = mod_pow(9726,3533,11413),
    4086 = mod_pow(5971,6597,11413),
    1028 = bsize(k_bit_random_integer(1028)),
    200 = bsize(k_bit_random_integer(200)),
    yes.

t1(B) ->
    I = bin2int(B),
    B = int2bin(I).

%% mod_pow(A, B, M) => (A^B) mod M
%% examples mod_pow(9726,3533,11413) = 5761
%%          mod_pow(5971,6597,11413) = 4086

mod_pow(A, 1, M) ->
    A rem M;
mod_pow(A, 2, M) ->
    A*A rem M;
mod_pow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = mod_pow(A, B1, M),
    case B2 of
	B1 -> (P*P) rem M;
	_  -> (P*P*A) rem M
    end.

%% inv(A, B) = C | no_inverse
%%    computes C such that
%%    A*C mod B = 1
%% computes A^-1 mod B
%% examples inv(28, 75) = 67.
%%          inv(3533, 11200) = 6597
%%          inv(6597, 11200) = 3533

inv(A, B) ->
    case solve(A, B) of
	{X, _} ->
	    if X < 0 -> X + B;
	       true  -> X
	    end;
	_ ->
	    no_inverse
    end.

%% solve(A, B) => {X, Y} | insoluble
%%   solve the linear congruence
%%   A * X - B * Y = 1

%S tag1
solve(A, B) ->
    case catch s(A,B) of
	insoluble -> insoluble;
	{X, Y} ->
	    case A * X - B * Y of
		1 -> {X, Y};
		_ -> error
	    end
    end.

s(_, 0)  -> throw(insoluble);
s(_, 1)  -> {0, -1};
s(_, -1) -> {0, 1};
s(A, B)  ->
    K1 = A div B,
    K2 = A - K1*B,
    {Tmp, X} = s(B, -K2),
    {X, K1 * X - Tmp}.
%E tag1


%% converts a string to a base 256 integer
%% converts a base 256 integer to a string

%% because there is a leading zero problem we stick an "z" on the beginning
%% and remove it when we take the inverse

%% START:str2int    
str2int(Str) -> str2int([$z|Str], 0).

str2int([H|T], N) -> str2int(T, N*256+H);
str2int([], N)    -> N.

int2str(N) -> 
    "z" ++ S = int2str(N, []),
    S.

int2str(N, L) when N =< 0 -> L;
int2str(N, L) ->
    N1 = N div 256,
    H  = N - N1 * 256,
    int2str(N1, [H|L]).
%% END:str2int    

int2bin(Int) ->
    S = int2str(Int),
    list_to_binary(S).

bin2int(B) -> str2int(binary_to_list(B)).

%% greatest common denominator

gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) ->
    gcd(B, A rem B).

%% decimal size
    
dsize(K) when K < 10 -> 1;
dsize(K) -> 1 + dsize(K div 10).

%% START:make_rsa_keypair
make_rsa_keypair(K) ->
    %% K is the bitsize of the modulo N
    K1 = K div 2,
    E = 65537,
    P = gen_prime(K1, E),
    Q = gen_prime(K-K1, E),
    N = P*Q,
    Phi = (P-1)*(Q-1),
    D = inv(E, Phi),
    {E,D,N}.
%% END:make_rsa_keypair

%% gen_prime(K, N)
%%   makes a prime of K bits that is relatively prime to N

gen_prime(K, N) ->
    P = ez_crypt_primes:make_k_bit_prime(K),
    case gcd(P, N) of
	1 -> P;
	_ -> gen_prime(K, N)
    end.

%% number of bits to represent N

bsize(0) -> 1;
bsize(1) -> 1;
bsize(K) -> 1 + bsize(K bsr 1).

k_bit_random_integer(K) ->
    %% makes a K bit random integer
    Len1 = trunc(K/8) + 2,
    %% start by making a larger number than necessary then removing
    %% some bits until it is the the correct size
    N = random_integer(Len1),
    InitialSize = bsize(N),
    if
	InitialSize =< K ->
	    io:format("** should not happen **~n"),
	    io:format("initial size:~p K:~p~n",[bsize(N), K]),
	    k_bit_random_integer(K);
	true ->
	    N1 = fixup_size(N, K),
	    %% set the last bit to 1
	    %% so it's odd
	    %% and if adding one mucks up the bit length
	    %% then do it all over again
	    %% total bit length
	    case is_even(N1) of
		true -> 
		    N2 = N1 + 1,
		    case bsize(N2) of
			K -> N2;
			_ ->
			    %% this is **very** unlikely
			    %% but we might as well get the code right
			    k_bit_random_integer(K)
		    end;
		false -> 
		    N1
	    end
    end.

%% Initially N is larger than K bits wide
%% reduce it one bit at a time until it is K bits wide

fixup_size(N, K) ->
    case ez_crypt_math:bsize(N) of
	K ->
	    N;
	B when B > K ->
	    fixup_size(N bsr 1, K)
    end.

random_integer(Len) ->
    random_integer(Len, 1000).

random_integer(_, 0) ->
    exit(too_may_tries);
random_integer(Len, K) ->
    case (catch strong_rand_bytes(Len)) of
	{'EXIT', _} ->
	    random_integer(Len, K-1);
	Bin ->
	    pint_to_int(Bin, 0)
    end.

pint_to_int(<<>>,N) -> N;
pint_to_int(<<X:8,B/binary>>,N) ->
    pint_to_int(B, N*256+X).

is_even(K) ->
    K band 1 == 0.






