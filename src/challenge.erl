-module(challenge).
-compile(export_all).

test() ->
    Server = spawn(?MODULE, server, []),
    _Client = spawn(?MODULE, client, [Server, "joe", self()]),
    receive X -> X end.

client(Server, Who, Parent) ->
    Server ! {self(), {login,Who}},
    receive
	{Server, {challenge, X}} ->
	    Server ! {self(), {response, 
			       erlang:md5(X ++ secret(Who))}},
	    receive
		{Server, Response} ->
		    Parent ! Response
	    end
    end.
	    
server() ->
    receive
	{Pid, {login,Who}} ->
	    Ran = binary_to_list(crypto:strong_rand_bytes(10)),
	    Pid ! {self(), {challenge, Ran}},
	    receive
		{Pid, {response, R}} ->
		    case erlang:md5(Ran ++ secret(Who)) of
			R ->
			    Pid ! {self(), login_ok};
			_ ->
			    Pid ! {self(), login_error}
		    end
	    end
    end.

secret("joe") ->
    "zzbingo".
