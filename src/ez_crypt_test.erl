-module(ez_crypt_test).
-compile(export_all).

test() ->
    simple_symmetric_encryption(),
    aes_test(),
    stream_encryption1(),
    stream_encryption2(),
    rsa_test().

simple_symmetric_encryption() ->
    Pad = symmetric:pad(),
    Text = "The quick brown fox jumped over the lazy dog.",
    %% encrypt the text
    C1 = symmetric:encrypt_0(Pad, Text),
    %% Test we can get it back
    Text = symmetric:encrypt_0(Pad, C1),
    %% If we do it twice we get the same result
    C1 = symmetric:encrypt_0(Pad, Text),
    %% LGC
    Password = "password1",
    %% encrypt
    C2  =  symmetric:encrypt_1(Password, Text),
    %% decrypt
    Text = symmetric:encrypt_1(Password, C2),
    %% always the same
    C2  =  symmetric:encrypt_1(Password, Text),
    %% Now salt the value
    C3  =  symmetric:encrypt_2(Password, Text),
    %% decrypt
    Text = symmetric:decrypt_2(Password, C3),
    %% Do a second time and check it's different
    C4  =  symmetric:encrypt_2(Password, Text),
    false = (C3 == C4), %% different !
    ok.

aes_test() ->
    Password = <<"secret">>,
    Data = make_binary(2000), %% random binary
    C1   = ez_crypt:aes_encrypt(Password, Data),
    Data = ez_crypt:aes_decrypt(Password, C1),
    C2   = ez_crypt:aes_encrypt(Password, Data),
    false = (C1 == C2),
    ok.

make_binary(K) ->
    crypto:strong_rand_bytes(K).

stream_encryption1() ->
    Password = <<"secret">>,
    S0 = ez_crypt:stream_init(Password),
    {S1,C1} = ez_crypt:stream_encrypt(S0, <<"hello ">>),
    {_, C2} = ez_crypt:stream_encrypt(S1, <<"world">>),
    %% we can take the fragments and decrypt them
    NS0 = ez_crypt:stream_init(Password),
    {NS1,B1} = ez_crypt:stream_encrypt(NS0, C1),
    true = (B1 == <<"hello ">>), 
    {_,B2} = ez_crypt:stream_encrypt(NS1, C2),
    true = (B2 == <<"world">>),
    ok.

stream_encryption2() ->
    Password = <<"secret">>,
    Text = <<"hello brave new world">>,
    S0 = ez_crypt:stream_init(Password),
    {_,C1} = ez_crypt:stream_encrypt(S0, Text),
    {B1, B2} = split_binary(C1, 4),
    %% recombine
    {Sa, T1} = ez_crypt:stream_decrypt(S0, B1),
    {_, T2} = ez_crypt:stream_decrypt(Sa, B2),
    Final = <<T1/binary,T2/binary>>,
    Final == Text.

%% START:rsa_test
rsa_test() ->    
    {E,D,N} = ez_crypt:make_rsa_key(800),
    Secret = 1234567890,
    Crypt  = ez_crypt:mod_pow(Secret,E,N),
    Secret = ez_crypt:mod_pow(Crypt,D,N),
    %% swap E and D
    Crypt1 = ez_crypt:mod_pow(Secret,D,N),
    Secret = ez_crypt:mod_pow(Crypt1,E,N),
    %% It's determanistic
    Crypt2 = ez_crypt:mod_pow(Secret,E,N),
    true = (Crypt2 == Crypt),
    yes.
%% END:rsa_test

rsa_test1(K) ->
    {E,D,N} = ez_crypt:make_rsa_key(1024),
    B = make_binary(K),
    C = ez_crypt:rsa_encrypt({D,N}, B),
    B = ez_crypt:rsa_decrypt({E,N}, C),
    yes.



    
