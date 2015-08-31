-module(ez_crypt_rsa).
-compile(export_all).

%% Do do
%%   refactor so we onlt call ez_crypt
%%   timing and size tests

test() ->
    {E,D,N} = keys(),
    Pub = {E,N},
    Pri = {D,N},
    Mega = mega_byte(),
    test1(<<"1234567890">>, Pub, Pri),
    test1(<<"12345678901234567890231739127397193712937917391273917397193719379173913792312379137917391739173971937197391739173971293719aaab">>, Pub, Pri),
    test2(<<"123456789">>, Pub, Pri),
    test3(<<"123456789">>, Pub, Pri),
    test3(Mega, Pub, Pri),
    yes.

mega_byte() ->
    B1 = list_to_binary(lists:seq(0,255)),
    list_to_binary(lists:duplicate(4096, B1)).

test1(Bin, Pub, Pri) ->
    I   = encrypt_1(Pub, Bin),
    Bin = decrypt_1(Pri, I).

test2(Bin, Pub, Pri) ->
    I   = encrypt_2(Pub, Bin),
    Bin = decrypt_2(Pri, I).

test3(Bin, Pub, Pri) ->
    I   = encrypt_3(Pub, Bin),
    Bin = decrypt_3(Pri, I).


%% Encrypt 1 = "text book RSA"

encrypt_1({E,N}, Bin) ->
    Str = binary_to_list(Bin),
    I   = ez_crypt_math:str2int(Str),
    if I =< N ->
	    ez_crypt_math:mod_pow(I, E, N);
       true ->
	    io:format("** size error in ez_crypt_rsa:encrypt_1 ~p ~p~n",
		      [I,N]),
	    exit({encrypt,size,I,N})
    end.

decrypt_1({D,N}, I) ->
    J = ez_crypt_math:mod_pow(I,D,N),
    list_to_binary(ez_crypt_math:int2str(J)).


%% Encrypt 2
%%  Text book RSA with OAEP Padding

%% in encrypt_2 we need to know the Modulo size it's 1024 bits
%%   1024 bits = 128 bytes to play with
%%                20 bytes is the random seed
%%                I'll set buffer to 120

%% Note to self I could call bit_size(N) on the modulo to find the 
%% bit size. The padded binary is actually not the value
%% used in encrypt_1. This has to be converted to an integer
%% and the converted integer has to fit into 1024 bits
%% so I'll set a lower value

encrypt_2(Key, Bin) when size(Bin) =< 120 ->
    Bin1 = oaep_byte_padding:pad(Bin, 120, 20),
    encrypt_1(Key, Bin1).

decrypt_2(Key, Bin) ->    
    Bin1 = decrypt_1(Key, Bin),
    oaep_byte_padding:unpad(Bin1, 20).
    
%% Now handle anything we can throw at it ...

encrypt_3(Key, Bin) ->
    %% make a session key 
    Session = ez_crypt:random_session_key(),
    %% encrypt the data with the session key
    EncData = ez_crypt_aes:encrypt(Session, Bin),
    %% encrypt the session key
    EncSession = encrypt_2(Key, Session),
    %% add a wrapper
    Bundle = term_to_binary({EncSession, EncData}),
    Bundle.

decrypt_3(Key, Bundle) ->
    %% remove the wrapper
    {EncSession, EncData} = binary_to_term(Bundle),
    %% decrypt the session key
    Session = decrypt_2(Key, EncSession),
    %% decrypt the data
    Data    = ez_crypt_aes:decrypt(Session, EncData),
    Data.

keys() ->
    {65537,
     12901971560344731623714892620385933851981310268248631655222013486395675434059634915505948561032167507464531809332480330508692843729145482423663282062688469573787107088973166278178315041330885466966016550799082934955881035405566463326776038417375846422826465488029991490557531730623662467154219829089630598553,
     74927470992495584973274516407818604063562173774941122976365538135393299151259751303279871585677019223456182648491162022201878856843332519415119230531007038455201812002954362836209633061518067731325346775671448557642921454091003472671059522636286504819845200123597163430897836418492236040080954911925921110687}.
