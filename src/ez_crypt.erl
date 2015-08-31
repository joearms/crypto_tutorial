-module(ez_crypt).

-export([
	 aes_decrypt/2,
	 aes_encrypt/2,
	 bsize/1,
	 decrypt_message/3,
	 encrypt_message/2,
	 file2sha/1,
	 is_probably_prime/1,
	 make_k_bit_prime/1,
	 make_rsa_key/1,
	 make_rsa_keyfiles/4,
	 mod_pow/3,
	 random_session_key/0,
	 read_private_key/2,
	 read_public_key/1,
	 rsa_decrypt/2,
	 rsa_encrypt/2,
	 sha1_checksum_of_file/1,
	 sign_current_dir/0,
	 sign_file/3,
	 stream_decrypt/2,
	 stream_encrypt/2,
	 stream_init/1,
	 validate_file/2,
	 validate_this_dir/0
	 ]).


-spec mod_pow(N::integer(), P::integer(), M::integer()) -> R::integer().
-spec make_k_bit_prime(Nbits::integer()) -> Prime::integer().

%% compute N^P mod N

mod_pow(N, P, M) -> ez_crypt_math:mod_pow(N, P, M).

make_k_bit_prime(K) -> ez_crypt_primes:make_k_bit_prime(K).

bsize(N) -> ez_crypt_math:bsize(N).

make_rsa_key(LenInBits) -> ez_crypt_math:make_rsa_keypair(LenInBits).

is_probably_prime(X) -> ez_crypt_miller_rabin:is_probably_prime(X).

%% START:make_rsa_keyfiles
make_rsa_keyfiles(Name, Email, Len, Password) ->
    {E,D,N} = make_rsa_key(Len),
    {ok, S} = file:open(Name ++ ".pub", [write]),
    io:format(S,"~p.~n",
	      [#{type=>public_key,
		 email => Email,
		 e => E,
		 n => N}]),
    file:close(S),
    Term = #{type => private_key,
	     name => Name,
	     email => Email,
	     e => E,
	     d => D,
	     n => N},
    Bin1 = term_to_binary(Term),
    Bin2 = aes_encrypt(Password, Bin1),
    {ok, S1} = file:open(Name ++ ".pri", [write]),
    io:format(S1,"~p.~n",
	      [#{type=>encrypted_private_key,
		 value => Bin2}]),
    file:close(S1).
%% END:make_rsa_keyfiles
-spec random_session_key() -> binary().

random_session_key() ->
    crypto:strong_rand_bytes(20).

sign_file(File, PrivateKeyFile, Password) ->
    SHA = sha1_checksum_of_file(File),
    PriKey = read_private_key(PrivateKeyFile, Password),
    Sig = rsa_encrypt(PriKey, SHA),
    file:write_file(File ++ ".sig", term_to_binary(Sig)).

validate_file(PubKeyFile,  File) ->
    SHA1 = sha1_checksum_of_file(File),
    {ok, Bin} = file:read_file(File ++ ".sig"),
    Sig = binary_to_term(Bin),
    PubKey = read_public_key(PubKeyFile),
    io:format("PubKey=~p~n",[PubKey]),
    SHA2 = rsa_decrypt(PubKey, Sig),
    Ret = case SHA1 of
	      SHA2 -> true;
	      _    -> false
	  end,
    io:format("Validate:~s ~p~n",[File, Ret]),
    Ret.

sha1_checksum_of_file(File) ->
    {ok, Bin} = file:read_file(File),
    sha1_checksum(Bin).

sha1_checksum(X) ->
    crypto:hash(sha, <<X/binary>>).

rsa_encrypt({E,N}=Key, Bin) when is_integer(E),
				 is_integer(N),
				 is_binary(Bin) ->
    ez_crypt_rsa:encrypt_3(Key, Bin).

rsa_decrypt({E,N}=Key, B) when is_integer(E),
			       is_integer(N),
			       is_binary(B) ->
    ez_crypt_rsa:decrypt_3(Key, B).

sign_current_dir() ->
    F = erl_files(),
    L = [{file,I,sha1_checksum_of_file(I)} || I <- F],
    io:format("L=~p~n",[L]),
    write_term("catalog", L),
    sign_file("catalog", "joe.pri", <<"verysecret">>).


erl_files() ->
    F = filelib:wildcard("*.erl"),
    lists:filter(fun(".#" ++ _) -> false;
		    (_) -> true
		 end, F).


validate_this_dir() ->
    validate_file("joe.pub","catalog").

write_term(File, X) ->
    {ok, H} = file:open(File,[write]),
    io:format(H, "~p.~n",[X]),
    file:close(H).

aes_encrypt(Password, Bin) when is_binary(Password),
				is_binary(Bin) ->
    ez_crypt_aes:encrypt(Password, Bin).

aes_decrypt(Password, Bin) when is_binary(Password),
				is_binary(Bin) ->
    ez_crypt_aes:decrypt(Password, Bin).

read_private_key(File, Password) ->
    {ok, [#{type := encrypted_private_key,
	    value := Bin}]} = file:consult(File),
    Bin1 = aes_decrypt(Password, Bin),
    Term = binary_to_term(Bin1),
    #{d := D, n:= N} = Term,
    {D, N}.

read_public_key(File) ->
    {ok, [#{type := public_key,
	    e := E, n := N}]} = file:consult(File),
    {E, N}.

encrypt_message(Who, Msg) ->
    Ran = random_session_key(),
    EncMessage = aes_encrypt(Ran, Msg),
    PubKey = read_public_key(Who),
    EncKey = rsa_encrypt(PubKey, Ran),
    term_to_binary({EncKey,EncMessage}).

decrypt_message(Who, Password, Bin) ->
    {EncKey,EncMessage} = binary_to_term(Bin),
    PriKey = read_private_key(Who, Password),
    Key = rsa_decrypt(PriKey, EncKey),
    Message = aes_decrypt(Key, EncMessage),
    Message.

stream_init(Password) ->
    crypto:stream_init(rc4, Password).

stream_encrypt(Key, Bin) ->
    crypto:stream_encrypt(Key, Bin).

stream_decrypt(Key, Bin) ->
    crypto:stream_decrypt(Key, Bin).

%% START:filehash
file2sha(File) ->
    hash_file(File, sha).

hash_file(File, Method) ->
    % the file can be huge so read it in chunks
    case file:open(File, [binary,raw,read]) of
	{ok, P} -> hash_loop(P, crypto:hash_init(Method));
	Error   -> Error
    end.

hash_loop(P, C) ->
    case file:read(P, 4096) of
	{ok, Bin} ->
	    hash_loop(P, crypto:hash_update(C, Bin));
	eof ->
	    file:close(P),
	    {ok, crypto:hash_final(C)}
    end.
%% END:filehash




    
    
    
