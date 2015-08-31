-module(decode_rsa_keys).
-compile(export_all).

-include_lib("public_key/include/public_key.hrl"). 

%% ssh-keygen -t rsa -b 1024 -C "joe@somewehere.com"
%% Generating public/private rsa key pair.
%% Enter file in which to save the key (/Users/joearmstrong/.ssh/id_rsa): joe_rsa
%% Enter passphrase (empty for no passphrase): 
%% Enter same passphrase again: 
%% Your identification has been saved in joe_rsa.
%% Your public key has been saved in joe_rsa.pub.

%% START:all
test() ->
    {E,D,N} = Key = read_pri_certificate(),
    {E,N}   = read_pub(),
    io:format("Key:~p~n",[Key]),
    false  = ez_crypt_primes:is_prime(N),
    check(12345, {E,D,N}),
    check(54321, {D,E,N}),
    1024 = ez_crypt_math:bsize(N),  %% modulus size
    wow.

check(Msg, {E,D,N}) ->
    Crypt  = ez_crypt_math:mod_pow(Msg, E, N),
    Decode = ez_crypt_math:mod_pow(Crypt, D, N),
    Msg = Decode,
    ok.

read_pri_certificate() ->
    {ok, B} = file:read_file("joe_rsa"),
    [DSA] = public_key:pem_decode(B),
    X = public_key:pem_entry_decode(DSA),
    #'RSAPrivateKey'{modulus = N, 
		     publicExponent = E, 
		     privateExponent=D} = X,
    {E,D,N}.

read_pub() ->
    {ok, B} = file:read_file("joe_rsa.pub"),
    [{Pub,_}] = public_key:ssh_decode(B,public_key),
    #'RSAPublicKey'{modulus = N, publicExponent = E} = Pub,
    {E, N}.
%% END:all


    

