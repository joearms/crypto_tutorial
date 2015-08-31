-module(tls).
-compile(export_all).

-import(ez_crypt,
	[random_session_key/0,
	 make_rsa_key/1
	]).

test() ->
    Server = spawn(tls, server, []),
    spawn(tls, client, [Server]).

client(Server) ->
    io:format("Client requesting key~n"),
    Server ! {self(), hello},
    receive
	{Server, ServerPub} ->
	    S1       = random_session_key(),
	    {E,D,N}  = make_rsa_key(1024),
	    TmpPri   = {D,N},
	    TmpPub   = {E,N},
	    Bin1     = term_to_binary({S1,TmpPub}),
	    Msg1     = {key1, rsa_encrypt(ServerPub, Bin1)},
	    io:format("Client sending   S1:~p~n",[S1]),
	    Server ! {self(),Msg1},
	    receive
		{Server, {key2, Int}} ->
		    S2 = rsa_decrypt(TmpPri, Int),
		    io:format("Client recovered S2:~p~n",[S2]),
		    client_loop(S1, S2)
	    end
    end.

%% Note very simple. Need to make sure the key and the Pub will fit
%% inside the 1024 bits whuch I did't check I just hoped it would be
%% OK (it was)

server() ->
  receive
    {Client, hello} ->
      io:format("Server sending public key~n"),
      Client ! {self(), server_public_key()},
      receive
	{Client, {key1,Int}} ->
	  Bin1 = rsa_decrypt(server_private_key(), Int),
	  {S1, TmpPub} = binary_to_term(Bin1),
	  io:format("Server recovered S1:~p~n",[S1]),
	  S2 = random_session_key(),
	  io:format("Server sending   S2:~p~n",[S2]),
	  Client ! {self(), {key2,rsa_encrypt(TmpPub, S2)}},
	  server_loop(S1, S2)
      end
  end.

rsa_encrypt(Key, Bin) ->
    ez_crypt:rsa_encrypt(Key, Bin).
    %% ez_crypt_rsa:encrypt_1(Key, Bin).

rsa_decrypt(Key, Bin) ->
    ez_crypt:rsa_decrypt(Key, Bin).
    %% ez_crypt_rsa:decrypt_1(Key, Bin).


%% To make this I evaluated ez_crypt:make_rsa_key(1024)
%% and cut and paste the results here

keys() ->
    {65537,
     12901971560344731623714892620385933851981310268248631655222013486395675434059634915505948561032167507464531809332480330508692843729145482423663282062688469573787107088973166278178315041330885466966016550799082934955881035405566463326776038417375846422826465488029991490557531730623662467154219829089630598553,
     74927470992495584973274516407818604063562173774941122976365538135393299151259751303279871585677019223456182648491162022201878856843332519415119230531007038455201812002954362836209633061518067731325346775671448557642921454091003472671059522636286504819845200123597163430897836418492236040080954911925921110687}.

server_private_key() ->
    {_,D,N} = keys(),
    {D, N}.

server_public_key() ->
    {E,_,N} = keys(),
    {E, N}.
				      
client_loop(_S1, _S2) ->
    %% io:format("in client loop:~p ~p~n",[S1,S2]),
    receive
    after infinity ->
	    true
	end.

server_loop(_S1, _S2) ->
    %% io:format("in server loop:~p ~p~n",[S1,S2]),
    receive
    after infinity ->
		true
	end.




