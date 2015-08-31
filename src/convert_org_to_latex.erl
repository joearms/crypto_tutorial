-module(convert_org_to_latex).
%% -compile(export_all).
-import(lists, [reverse/1]).

-export([test/0]).

%% Note: rapidly hacked together - not beautiful
%% no documentation

%% first step break the entire thing into lines

test() ->
    io:format("yes~n"),
    transform("./crypto_latex.org", "expand.tex").

transform(In, Out) ->
    io:format("Transforming:~p~n",[In]),
    Lines = elib2_misc:file2lines(In),
    Blocks0 = parse(Lines, []),
    _Root = filename:rootname(In),
    %% elib2_misc:dump(Root++"_blocks0.tmp", Blocks0),
    Blocks1 = expand_cmds(Blocks0),
    %% elib2_misc:dump(Root++"_blocks1.tmp", Blocks1),
    Blocks2 = pass1_code_includes(Blocks1),
    %% elib2_misc:dump(Root++"_blocks2.tmp", Blocks2),
    Blocks4 = merge_li(Blocks2, []), 
    %% elib2_misc:dump(Root++"_blocks4.tmp", Blocks4),
    Z = to_tex(Blocks4),
    elib2_misc:check_io_list(Z),
    file:write_file(Out, Z),
    io:format("Written: ~s~n",[Out]).

parse(["** " ++ H|T], L) -> parse(T, [{h2,remove_trailing_nl(H)}|L]);
parse(["* "  ++ H|T], L) -> parse(T, [{h1,remove_trailing_nl(H)}|L]);
parse(["+ "  ++ H|T], L) -> parse(T, [{li,H}|L]);
parse([">"   ++ H|T], L) -> parse(T, [{cmd,H}|L]);
parse(["!!"   ++ H|T], L) -> 
    Val = do_bangs(H),
    parse(T, [Val|L]);
parse(["    "   ++ H|T], L) -> 
    {Pre, T1} = get_pre(T, [H]),
    parse(T1, [Pre|L]);
parse(["\\begin{verbatim}"   ++ H|T], L) -> 
    {Pre, T1} = get_verbatim(T, [H]),
    parse(T1, [Pre|L]);

parse([H|T], L)          -> parse(T, [{str,H}|L]);
parse([], L)             -> reverse(L).

merge_li([{li,_}|_]=A, L) ->
    {Li, B} = lists:splitwith(fun is_li/1, A),
    merge_li(B, [{ul,Li}|L]);
merge_li([H|T], L) ->
    merge_li(T, [H|L]);
merge_li([], L) ->
    reverse(L).

do_bangs(X) ->
    X1 = elib2_misc:skip_blanks(X),
    case string:tokens(X1,":") of
	["include_tagged",File,Tag|_] ->
	    Str = get_erl_section(File, Tag),
	    {boxed, File, Str};
	Y ->
	    io:format("Cannot do ~p~n",[Y]),
	    exit(1)
    end.


pass1_code_includes(B) ->
    %% do the code includes
    L = [{M,F,A} || {include_function,M,F,A} <- B],
    %% parse all the modules
    Mods = [M || {M,_,_} <- L],
    Mods1 = elib2_misc:remove_duplicates(Mods),
    io:format("Mods1:~p~n",[Mods1]),
    L1 = [{F, segment(F)} || F <- Mods1],
    [do_includes(I, L1) || I <- B].

segment(File) ->
    case filelib:is_file(File) of
	true ->
	    segmenting:segment(File);
	false ->
	    io:format("Missing file:~p~n",[File]),
	    error
    end.

do_includes({include_function,File,F,A}, L) -> 
    case (catch fetch_code(File,F,A,L)) of 
	{'EXIT', Why} ->
	    io:format("Cannot find:~p/~p in ~p::~p~n",[F,A,File,Why]),
	    {pre,"**** error ***"};
	Toks ->
	    {erl_tokens, #{file => File, 
			   func => F, 
			   arity=> A, 
			   toks => Toks}}
    end;
do_includes(X, _) -> 
    X.

fetch_code(File, Func, Arity, L) ->
    {value,{_, S}} = lists:keysearch(File, 1, L),
    {value,{_, Val}} = lists:keysearch({function,{Func,Arity}}, 1, S),
    Val.

add_if_non_nil([], L) -> L;
add_if_non_nil(X, L)  -> [X|L]. 
    
get_verbatim(["\\end{verbatim}" ++ _|T], L) ->
    {{pre, lists:append(reverse(L))}, T};
get_verbatim([H|T], L) ->
    get_verbatim(T, [H|L]);
get_verbatim([], L) ->
    {{pre, lists:append(reverse(L))}, []}.
    

get_pre([], L) ->
    {make_pre(L), []};
get_pre([H|T]=A, L) ->
    case is_blank(H) of
	true ->
	    get_pre(T, ["\n"|L]);
	false ->
	    case H of
		"    " ++ Two ->
		    get_pre(T, [Two|L]);
		_ ->
		    {make_pre(L), A}
	    end
    end.

make_pre(L) ->
    Content = lists:append(reverse(L)),    %% {node,pre,#{}, Content}.
    {pre, Content}.


is_blank([$\s|T]) -> is_blank(T);
is_blank([$\n|T]) -> is_blank(T);
is_blank([])      -> true;
is_blank(_)       -> false.


%%----------------------------------------------------------------------

remove_trailing_nl([$\n]) -> [];
remove_trailing_nl([H|T]) -> [H|remove_trailing_nl(T)]; 
remove_trailing_nl([])    ->  [].
    
%%----------------------------------------------------------------------

expand_cmds(L) ->
    LF = {eval, fun local/3},
    P = myshell:start(LF),
    expand_cmds(L, P, 1, []).

expand_cmds([{cmd,"#" ++ X}|T], P, K, L) ->
    %% # command: Exprs is evaluated and the result inserted
    %% into the stream
    case cmd(P, X) of
	{ok, Val} ->
	    L1 = add_if_non_nil(Val, L),
	    expand_cmds(T, P, K, L1);
	{error, E} ->
	    io:format("Command failed:~p~n",[E]),
	    exit({badCommand,X})
    end;
expand_cmds([{cmd, "!" ++ X}|T], P, K, L) ->
    %% evaluate the command and throw away the
    %% result
    Obj = string2exprs(X),
    case Obj of
	[{atom,_,new}] ->
	    %% reset the bindings and set the command
	    %% number to 1
	    myshell:stop(P),
	    P1 = myshell:start({eval, fun local/3}),
	    expand_cmds(T, P1, 1, L);
	_Exprs ->
	    cmd(P, X),
	    expand_cmds(T, P, K, L)
	end;
expand_cmds([{cmd, X}|T], P, K, L) ->
    %% Print the command, then evaluate the command 
    %% print the results
    Str = case cmd(P, X) of
	      {ok, Val} ->
		  lists:flatten(io_lib:format("~p~n",[Val]));		  
	      {error, EBin} ->
		  binary_to_list(EBin)
	  end,
    expand_cmds(T, P, K+1, [{pre,integer_to_list(K) ++ ">" ++ X  
			      ++ Str}|L]);
expand_cmds([H|T], P, K, L) ->
    expand_cmds(T, P, K, [H|L]);
expand_cmds([], P, _, L) ->
    myshell:stop(P),
    reverse(L).

cmd(P, X) ->
    myshell:cmd(P,X).


string2exprs(Str) ->
    case erl_scan:tokens([], Str, 1) of
	{done, {ok, Toks, _}, []} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok, Exprs} ->
		    Exprs;
		{error,{Line,Mod,Arg}} ->
		    EStr = io_lib:format("~s",[apply(Mod,format_error,[Arg])]),
		    Msg = lists:flatten(EStr),
		    io:format("~n***PARSE ERROR in line:~w ~s~n", [Line,Msg]),
		    io:format("Str=~s~n",[Str]),
		    exit({badExpr,Str})
		end;
	Other ->
	    io:format("~n***SCAN ERROR:~p~n", [Other]),
	    error
    end.

eval_erl(Exprs, B0) ->
    case (catch erl_eval:exprs(Exprs, B0, {eval, fun local/3})) of
	{'EXIT', Why} ->
	    io:format("Error:~p~n",[Why]),
	    {value, "<p>** error see the log for why **</p>", B0};
	{redirect, X} ->
	    throw({redirect,X});
	Other ->
	    io:format("Here:~p~n",[Other]),
	    Other
    end.

local(def,[{atom,_,Name},{integer,_,Arity},{'fun',_,{clauses,Clauses}}], B0) ->
    put({func,Name,Arity}, Clauses),
    {value, true, B0};
local(bindings, [], B0) ->
    {value, B0, B0};
local(pre, [Expr], B0) ->
    {value, Val, B1} = eval_erl([Expr], B0),
    {value, pre(Val), B1};
local(show_all_bindings,[],B0) ->
    {value, pre(B0), B0};
local(show_bindings,[],B0) ->
    B1 = lists:sort(lists:keydelete('SYS', 1, B0)),
    {value, ["<h1>Bindings</h1>",pre(B1)], B0};
local(redirect, [Expr], B0) ->
    {value, Val, _B1} = eval_erl([Expr], B0),
    io:format("Redirecting to:~p~n",[Val]),
    throw({redirect, Val});
local(Name, Args, B0) ->
    %% io:format("Local:::::~p~n",[{Name,Args,B0}]),
    LF = {eval, fun local/3},
    {Vals, _B1} =  eval1(Args, B0, LF),
    %% io:format("Vals=~p~n",[Vals]),
    CCC = apply(orgmode_local, Name, Vals),
    %% io:format("CCC=~p~n",[CCC]),
    {value, CCC, B0}.

%% local(Name, Args, B0) ->
%%     io:format("Local:~p~n",[{Name,Args,B0}]),
%%     Arity = length(Args),
%%     Clauses = get({func,Name,Arity}),
%%     LF = {eval, fun local/3},
%%     {Vals, _B1} =  eval1(Args, B0, LF),
%%     case erl_eval:match_clause(Clauses, Vals, [], LF) of
%% 	{Body, Bs1} ->
%% 	    {value, ZZ, _} = erl_eval:exprs(Body, Bs1, LF),
%% 	    {value, ZZ, B0};
%% 	nomatch ->
%% 	    erlang:error({function_clause,[{local,Name,Args}]})
%%     end.

eval1([H|T], B0, LF) ->
    {value, H1, B1} = erl_eval:expr(H, B0, LF),
    {T1, B2} = eval1(T, B1, LF),
    {[H1|T1], B2};
eval1([], B, _) ->
    {[], B}.
    
pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

is_li({li,_}) -> true;
is_li(_) ->  false.
    

%%----------------------------------------------------------------------

to_tex([{h1,X}|T]) ->
    ["\\chapter{",string2latex(X),"}\n" | to_tex(T)];
to_tex([{h2,X}|T]) ->
    ["\\section{",string2latex(X),"}\n" | to_tex(T)];
to_tex([{str,X}|T]) ->
    [X,to_tex(T)];
to_tex([{ul,L}|T]) ->
    ["\\begin{itemize}\n",
     to_tex(L),
     "\\end{itemize}\n",
     to_tex(T)];
to_tex([{li, X}|T]) ->
    ["\\item ", X, to_tex(T)];
to_tex([{pre, X}|T]) ->
    ["\\begin{verbatim}", X, "\\end{verbatim}\n", to_tex(T)];
to_tex([{boxed, File, X}|T]) ->
    %% very hacky -- but it works
    ["\\hfill\\erlFileName{",quote_under(File),"}",
     "{\\parskip=0pt\\begin{Verbatim}[frame=single]\n", 
     X, 
     "\\end{Verbatim}\n}\n", to_tex(T)];
to_tex([{erl_tokens, #{toks:=X,file:=File}}|T]) ->
    %% io:format("Tokens:~p~n",[X]),
    Vals = [Str || {_Tag,Str} <- X],
    to_tex([{boxed,File,Vals}|T]);
to_tex([{img,#{src:=File, height:=H}}|T]) ->
    ["\n\n \\includegraphics[height=",H,"]{",File,"}\n\n",to_tex(T)];
to_tex([]) ->
    [].

quote_under("_" ++ T) -> "\\_" ++ quote_under(T);
quote_under([H|T]) -> [H|quote_under(T)];
quote_under([]) -> [].

    

-spec string2latex(string()) -> string().

string2latex("%" ++ T)     -> "\\%" ++ string2latex(T);
string2latex("$" ++ T)     -> "\\$" ++ string2latex(T);
string2latex("#" ++ T)     -> "\\#" ++ string2latex(T);
string2latex("{" ++ T)     -> "\\{" ++ string2latex(T);
string2latex("}" ++ T)     -> "\\}" ++ string2latex(T);
string2latex("_" ++ T)     -> "\\_" ++ string2latex(T);
string2latex("<" ++ T)     -> "$<$" ++ string2latex(T);
string2latex(">" ++ T)     -> "$>$" ++ string2latex(T);
string2latex("\\" ++ T)     -> "\\\\" ++ string2latex(T);
string2latex("&lt;" ++ T)  -> "$<$" ++ string2latex(T);
string2latex("&amp;" ++ T) -> "\\&" ++ string2latex(T);
string2latex("&" ++ T)     -> "\\&" ++ string2latex(T);
string2latex([{ent,"ldquo"}|T]) -> "``" ++ string2latex(T);
string2latex([{ent,"rdquo"}|T]) -> "''" ++ string2latex(T);

string2latex([{em,X}|T]) -> 
    X1 = string2latex(X),
    ["{\\it ",X1,"}", string2latex(T)];
string2latex([{yellow,X}|T]) -> 
    X1 = string2latex(X),
    ["{\\colorbox{yellow}{",X1,"}", string2latex(T)];
string2latex([{strike,X}|T]) -> 
    X1 = string2latex(X),
    ["{\\sout{ ",X1,"}", string2latex(T)];
string2latex([{code,X}|T]) ->
    %% fixme if we have a plus
    ["{\\verb+",X,"+", string2latex(T)];
string2latex([H|T]) when is_integer(H)       -> [H|string2latex(T)];
string2latex([])           -> [].



-spec get_erl_section(FileName::string(), Tag::string()) -> string().

get_erl_section(File, Tag) ->
    case file:read_file(File) of
	{ok, B} ->
	    Stuff = get_stuff(binary_to_list(B), File, Tag),
	    Stuff1 = expand_tabs(Stuff),
	    %% io:format("Stuff=~p~n",[Stuff2]),
	    Stuff1
	    ;
	{error, _} ->
	    Str = lists:flatten(io_lib:format(
				  "*** ERROR: Missing File: ~s~n",[File])),
	    exit({eGetErlSection,File,Str})
    end.

get_stuff(Str, _, all) -> Str;
get_stuff(Str, File, Tag) -> get_stuff1(Str, File, Tag).

get_stuff1("%% START:" ++ T, File, Tag) ->
    case matches(T, Tag) of
	{yes, T1} -> get_content(T1, File, Tag, []);
	no        -> get_stuff1(T, File, Tag)
    end;
get_stuff1([_|T], File, Tag) ->
    get_stuff1(T, File, Tag);
get_stuff1([], File, Tag) ->
    throw({eGetErlSection, noStartTag, File, Tag}).

get_content("%% START:" ++ T, File, Tag, L) ->
    get_content(skip_to_nl(T), File, Tag, L);
get_content("%% END:" ++ T, File, Tag, L) ->
    case matches(T, Tag) of
	{yes, _} ->
	    reverse(L);
	no ->
	    get_content(skip_to_nl(T), File, Tag, L)
    end;
get_content([H|T], File, Tag, L) ->
    get_content(T, File, Tag, [H|L]);
get_content([], File, Tag, _) ->
    throw({eGetErlSection, eofInTag, File, Tag}).

matches(T, [])          -> {yes, skip_to_nl(T)};
matches([H|T1], [H|T2]) -> matches(T1, T2);
matches(_, _)           -> no.

expand_tabs(Xs) ->  expand_tabs(0,Xs).

%% N is the current column
expand_tabs(_N,[]) ->
    [];
expand_tabs(_, [$\n|T]) ->
    [$\n|expand_tabs(0, T)];
expand_tabs(N,[$\t|Xs]) ->
    N1 = 8*(N div 8) + 8,
    [$\s || _ <- lists:seq(N,N1-1)] ++ expand_tabs(N1,Xs);
expand_tabs(N,[X|Xs]) ->
    [X|expand_tabs(N+1,Xs)].
%%----------------------------------------------------------------------
%% @doc skip all characters up to and including new line

-spec skip_to_nl(string()) -> string().
    
skip_to_nl([$\n|T]) -> T;
skip_to_nl([])      -> [];
skip_to_nl([_|T])   -> skip_to_nl(T).
