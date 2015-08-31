-module(sfs).
-compile(export_all).

client(Server, SHA) ->
    Server ! {self(), public_key_request},
    receive
	{Server, {public_key_response, PubKey}} ->
	    case sha1(PubKey) of
		SHA ->
		    connect(Server, PubKey);
		_  ->
		    exit(badKey)
	    end
    end.

server() ->
    receive
	{Client, public_key_request} ->
	    Client ! {self(),
		      {public_key_response,
		       public_key()}},
	    wait_connect(Client)
    end.

public_key()    -> not_yet_implemented.
wait_connect(_) -> not_yet_implemented.
connect(_,_)    -> not_yet_implemented.
sha1(_)         -> not_yet_implemented.
