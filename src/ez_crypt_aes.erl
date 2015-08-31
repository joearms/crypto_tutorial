-module(ez_crypt_aes).

%% Warning - this code is for illustration purposes only
%% and has not been subject to a security review

-export([test/0, encrypt/2, decrypt/2]).

-spec encrypt(Password :: binary(), Plain :: binary()) ->
		     Code :: binary().

-spec decrypt(Password::binary(), Code::binary()) ->
		     Plain::binary().

test() ->
    test1(),
    test2(),
    test4(),
    ok.

test1() ->
    Password = <<"hello">>,
    Plain = <<"this is plain text">>,
    Code = encrypt(Password, Plain),
    io:format("Code=~p ~p~n",[Code,size(Code)]),
    Plain = decrypt(Password, Code).

test2() ->
    for(1,1000,
	fun(I) ->
		test3(I)
	end).

test3(I) ->
    Password = rand_password(),
    Plain    = crypto:rand_bytes(I),
    Code     = encrypt(Password, Plain),
    Plain    = decrypt(Password, Code).

for(N,N,F)     -> F(N);
for(I, Max, F) -> F(I), for(I+1,Max,F).
    
test4() ->
    Password = rand_password(),
    Plain    = crypto:rand_bytes(1000000),
    {T, _} = timer:tc(?MODULE, encrypt, [Password, Plain]),
    io:format("1 MByte encrypted in ~w ms~n", [T div 1000]).
    
rand_password() -> crypto:rand_bytes(10).

decrypt(Password, <<IV:16/binary,Bin1/binary>>) when is_binary(Password) ->
    P = erlang:md5(Password),
    Bin2 = crypto:block_decrypt(aes_cbc128, P,IV,Bin1),
    unpad(Bin2).

unpad(B) ->
    Size = size(B),
    {_, B2} = split_binary(B, Size - 1),
    [Pad] = binary_to_list(B2),
    Len = case Pad of
	      0 ->
		  %% the entire last block is padding
		  Size - 16;
	      _ ->
		  Size - Pad
	  end,
    {Bfinal, _} = split_binary(B, Len),
    Bfinal.

encrypt(Password, Bin) when is_binary(Password),is_binary(Bin) ->
    P = erlang:md5(Password),
    IV = crypto:rand_bytes(16),
    %% or strong_rand_bytes ????
    Bin1 = pad(Bin),
    %% io:format("~p~n",[{pass,P,size(P),
    %% 		       iv,IV,size(IV),bin1,Bin1,size(Bin1)}]),
    Bin2 = crypto:block_encrypt(aes_cbc128, P,IV,Bin1),
    <<IV/binary, Bin2/binary>>. 

pad(Bin) ->
    Extra = 16  - (size(Bin) rem 16),
    %% io:format("S:~p E:~p~n",[size(Bin),Extra]),
    pad(Extra, Bin).

%% pad(K, Bin) -> Bin1
%%   K = 0..15 number of bytes to pad by

pad(0, Bin) ->
    %% have to add 15 random bytes and then a zero
    B1 = crypto:rand_bytes(15),
    <<Bin/binary,B1/binary,0>>;
pad(K, Bin) ->
    B1 = crypto:rand_bytes(K-1),
    <<Bin/binary,B1/binary,K>>.
