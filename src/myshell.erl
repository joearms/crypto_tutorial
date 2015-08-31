-module(myshell).
-compile(export_all).

%% cmd(P, "XXX") -> {ok, "RRR"} | {error, "..."}

test() ->
    LF = {eval, fun local/3},
    P = start(LF),
    c(P, "X=1."),
    c(P, "Y=2."),
    c(P, "{X+Y,Y}."),
    c(P, "X=2."),
    c(P, "{X,Y,Y}."),
    stop(P),
    P1 = start(LF),
    c(P1, "Z=1."),
    c(P1, "{Z+Z,{a,Z}}."),
    c(P1, "a,b,c,d."),
    stop(P1).

local(_M,_F,_A) ->
    exit(local).

c(P, CmdStr) ->
    Out = cmd(P, CmdStr),
    io:format("> ~s~n~p~n",[CmdStr, Out]).

start(LF) -> spawn(myshell, go, [LF]).

stop(Pid) ->
    Ref = erlang:make_ref(),
    Pid ! {stop, self(), Ref},
    receive
	{Ref, Ret} ->
	    Ret
    end.


go(LF) ->
    B = erl_eval:new_bindings(),
    loop(B, LF).

loop(B0, LF) ->
    receive
	{stop, From, Ref} ->
	    From ! {Ref, stopped};
	{cmd, From, Ref, CmdStr} ->
	    try
		{ok, Tokens, _} = erl_scan:string(CmdStr),
		%% many forms can be comma-separated
		{ok, Forms} = erl_parse:parse_exprs(Tokens),
		%% eval individually
		{value, Val, B1} = erl_eval:exprs(Forms, B0, LF),
		From ! {Ref, {ok, Val}},
		loop(B1, LF)
	    catch
		T:R ->
		    S = erlang:get_stacktrace(),
		    Str1 = report_exception(T, serious, {R, S}, 1),
		    Str2 = list_to_binary(Str1),
		    From ! {Ref, {error,Str2}},
		    loop(B0, LF)
	    end
    end.

cmd(Pid, InStr) ->
    Ref = erlang:make_ref(),
    Pid ! {cmd, self(), Ref, InStr},
    receive
	{Ref, Ret} ->			       
	    Ret
    end.

report_exception(Class, Severity, {Reason,Stacktrace}, _RT0) ->
    Tag = shell:severity_tag(Severity),
    %% Tag = <<*>> <**>> <<"**">>
    I = iolist_size(Tag) + 1,
    RT = ets:new(a,[]),
    PF = fun(Term, I1) -> 
		 shell:pp(Term,I1,RT)
	 end,
    SF = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    Enc = shell:encoding(),
    Str = lib:format_exception(I, Class, Reason, Stacktrace, SF, PF, Enc),
    Str1 = [Tag,Str],
    lists:flatten(Str1).




