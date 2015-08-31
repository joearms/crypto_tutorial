-module(demo).

-compile(export_all).

%% P = demo:make_prime(50).
%% Q = demo:make_prime(50).
%% {E,D,N} = demo:make_key(P, Q)
%% M = 12345.
%% Enc = lin:pow(M,E,N).
%% lin:pow(Enc,D,N).

%% check a prime from the audience
%% is_prime
%% demo:is_prime(9746347772161). (carmichael number)



prime_from(N) ->
    N1 = case is_even(N) of
	     true  -> gen_prime(N+1);
	     false -> gen_prime(N)
	 end,
    io:format("\n"),
    N1.
    
gen_prime(N) ->
    case miller_rabin:is_probably_prime(N) of
	true  -> N;
	false -> 
	    io:format("."),
	    gen_prime(N+1)
    end.

is_even(K) ->
    K band 1 == 0.

str2int(Str) -> lin:str2int(Str).

int2str(N) -> lin:int2str(N).

is_prime(P) ->
    case miller_rabin:is_probably_prime(P) of
	true -> probably;
	false-> false
    end.

make_prime(N) ->
    %% N is in bits
    N1 = N * 3.3219,
    miller_rabin:make_prime(N1).

make_key(P, Q) ->
    N = P * Q,
    Phi = (P-1) * (Q-1),
    E = 65537,
    D = lin:inv(E, Phi),
    {E,D,N}.
