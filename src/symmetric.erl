-module(symmetric).

-compile(export_all).

%% https://en.wikipedia.org/wiki/Linear_congruential_generator

%% 1> symmetric:encrypt_1("password", "hello joe").
%%    [78,194,191,93,242,21,230,117,141]
%% 2> symmetric:encrypt_1("password", "hello joe").
%%    [78,194,191,93,242,21,230,117,141]

%% 3> symmetric:encrypt_2("password","hello joe").
%%    [53,39,155,160,85,223,148,236,31,87,240,18,5,186,151,237,33,169,206]
%% 4> symmetric:encrypt_2("password","hello joe").
%%    [152,15,217,156,37,174,97,136,47,207,188,187,23,77,203,209,156,185,61]

test() ->
    Txt = "the quick brown fox jumps over the lazy dog",
    C0  = encrypt_0(Txt, pad()),
    Txt = encrypt_0(C0, pad()),
    C   = encrypt_1("password", Txt),
    Txt = encrypt_1("password", C),
    C1  = encrypt_2("password", Txt),
    Txt = decrypt_2("password", C1).

%% START:pad
pad() ->
    "12ishiyr72TY873TGKY8HHAE7YT8YsadaHGFIYLIasdjasdgjasgd".

encrypt_0([H|T], [H1|T1]) ->
    [H bxor H1 | encrypt_0(T, T1)];
encrypt_0(_, []) ->
    [].
%% END:pad

%% START:encrypt_1
encrypt_1(Password, Str) ->
    X0 = password_to_int(Password),
    encrypt_1(X0, Str, []).

password_to_int(Str) ->
    erlang:phash(Str, 4294967296).

encrypt_1(X0, [H|T], L) ->
    X1  = lcg(X0),    
    Ran = next_byte(X0),
    encrypt_1(X1, T, [H bxor Ran|L]);
encrypt_1(_, [], L) ->
    lists:reverse(L).

next_byte(N) ->
    (N bsr 4) band 255.

lcg(X) ->
    A = 1664525,
    C = 1013904223,
    M = 4294967296,   %% 2^32
    (X*A + C) rem M.
%% END:encrypt_1

encrypt_2(Password, Txt) ->
    Salt = random_session_key(),
    X0   = password_to_int(Salt ++ Password),
    encrypt_1(X0, Txt, lists:reverse(Salt)).

decrypt_2(Password, Str) ->
    {Salt, Str1} = lists:split(10, Str),
    X0 = password_to_int(Salt ++ Password),
    encrypt_1(X0, Str1, []).

random_session_key() ->
    binary_to_list(crypto:strong_rand_bytes(10)).



    
