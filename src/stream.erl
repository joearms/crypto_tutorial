-module(stream).
-compile(export_all).

%% START:test
test() ->
    Password = <<"secret">>,
    Server = spawn(?MODULE, server, [Password, self()]),
    Client = spawn(?MODULE, client, [Password, Server]),
    Client ! {send_server, <<"hello">>},
    Client ! {send_server, <<" world">>},
    Client ! stop,
    Got = receive X -> X end,
    true = (Got == <<"hello world">>),
    hooray.
%% END:test

%% START:client
client(Password, Server) ->
    S0 = crypto:stream_init(rc4, Password),
    client_loop(S0, Server).

client_loop(S0, Server) ->
    receive
	{send_server, Bin} ->
	    {S1, B1}= crypto:stream_encrypt(S0, Bin),
	    Server ! {data, B1},
	    client_loop(S1, Server);
	stop->
	    Server ! stop
    end.
%% END:client

%% START:server
server(Password, Parent) ->
    S0 = crypto:stream_init(rc4, Password),
    Parent ! server_loop(S0, <<>>).

server_loop(S0, B) ->
    receive
	{data, Bin} ->
	    {S1, B1} = crypto:stream_decrypt(S0, Bin),
	    server_loop(S1, <<B/binary, B1/binary>>);
	stop ->
	    B
    end.
%% END:server

