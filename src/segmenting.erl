-module(segmenting).
%% this is the module comment

-compile(export_all).
-import(lists,[reverse/1]).

%% crude attempt to segment a file
%% We only consider files that are compilable
%% The first step is to parse all forms etc.

test() ->
    segment("./segmenting.erl").

%% this comment is for erl2emod

-spec erl2emod(string()) -> boolean().

erl2emod(File) ->
    J = segment(File),
    elib1_misc:dump(Z = File ++ ".tmp", J),
    io:format("look in:~p~n",[Z]),
    K = [reconstruct(I) || I <- J],
    elib1_misc:dump(File ++ ".emod.tmp", K).

reconstruct({{module, M}, _}) ->
    {module, M};
reconstruct({comment,L}) ->
    {comment, flatten_comment(L)};
reconstruct({{compile,export_all}, _}) ->
    {export_all, true};
reconstruct({{import,I},_}) ->
    {import, I};
reconstruct({{Tag,_}=F,Body}) when Tag == spec; Tag == function ->
    B1 = flatten_body(Body),
    {F, B1}.

flatten_body(L) ->
    list_to_binary([fb(I) || I <- L]).

fb({_Tag,Str}) -> Str.


flatten_comment(L) ->
    list_to_binary([fc(I) || I <- L]).

fc({str,S}) -> S;
fc({white_space,W}) -> W. 
    
start() ->
    L1 = filelib:wildcard(elib2_misc:root_dir() ++ "/lib/src/*.erl"),
    %% L2 = filelib:wildcard(code:lib_dir() ++ "/*/src/*.erl"),
    [segment(I) || I<- L1].

%% Here are some test cases

segment(File) ->
    %% io:format("segment:~p~n",[File]),
    Z = read_raw_forms(File),
    %5 elib2_misc:dump("raw_forms.tmp",Z),
    Z1 = partition(Z),
    %% elib2_misc:dump("raw_forms1.tmp",Z1),
    Segments = [zapit(I) || I <- Z1],
    %% elib2_misc:dump("segments.tmp",Segments),
    Reduced = reduce_segments(Segments, []),
    %% elib2_misc:dump("reduced.tmp",Reduced),
    Reduced.

partition(L) -> partition(L, []).
    
partition([H|T], L) ->
    L1 = add_forms(H, L),
    partition(T, L1);
partition([], L) ->
    reverse(L).

add_forms([], L) ->
    L;
add_forms(T, L) ->
    {T1, Form} = split_form(T), 
    add_forms(T1, [Form|L]).
   
split_form([{white_space,_,_}=H|T]) -> split_ws(T, [H]);
split_form([{comment,_,_}=H|T])     -> split_ws(T, [H]);
split_form(X)                       -> collect_form(X, []).

split_ws([{white_space,_,_}=H|T], L) -> split_ws(T, [H|L]);
split_ws([{comment,_,_}=H|T], L)     -> split_ws(T, [H|L]);
split_ws(X, L)                       -> {X, {ws, reverse(L)}}.

collect_form([{dot,_}=H|T], L) -> {T, {form,reverse([H|L])}};
collect_form([H|T], L)         -> collect_form(T, [H|L]).

collect_segments([{Min,Max,Tag}|T], I, Lines, L) ->
    {Segment, I1, Lines1} = collect_segement(Min,Max,I,Lines,[]),
    collect_segments(T, I1, Lines1, [{Tag,Min,Max,Segment}|L]);
collect_segments([],_,_,L) ->
    reverse(L).

collect_segement(Min,Max,I,[_|T], B) when I<Min ->
    collect_segement(Min,Max,I+1,T, B);
collect_segement(Min,Max,I,[H|T], B) when Min =< I, I =< Max ->
    collect_segement(Min, Max, I+1, T, [H|B]);
collect_segement(_, Max, I, T, B) when I > Max ->
    {reverse(B), I, T}.

zapit({ws,_}=X) -> X;
zapit({form,Toks}) ->
    Toks2 = remove_whitespace(Toks),
    Parse = erl_parse:parse_form(Toks2),
    %% summarise classifies the parse tree 
    %% fallback is used if the form is illegal and cannot be used
    Val = case Parse of
	      {ok, Form} -> summarise(Form);
	      {error, _} -> fallback_method(Toks2)
	  end,
    Toks3 = [prune(I) || I <- Toks],
    Toks4 = find_func_calls(Toks3),
    {form1, Val, Toks4}.
    
find_func_calls([{atom,X},{'(',"("}|T]) ->
    [{func_symbol,X},{'(',"("}|find_func_calls(T)];
find_func_calls([{atom,M},{':',":"},{atom,F},{'(',"("}|T]) ->
    [{mod_symbol,M},{':',":"},{func_symbol,F},{'(',"("}|find_func_calls(T)];
find_func_calls([H|T]) ->
    [H|find_func_calls(T)];
find_func_calls([]) ->
    [].

prune({Tag, L}) -> {Tag,proplists:get_value(text,L)};
prune({Tag,L,_}) -> {Tag, proplists:get_value(text,L)}.

summarise({attribute,_,spec,{S,_}})  -> {spec, S};
summarise({attribute,_,Tag,Val})     -> {Tag,Val};
summarise({function,_,Name,Arity,_}) -> {function,{Name,Arity}};
summarise(X)                         -> {cannot_summarise,X}.

fallback_method([{'-',_},{atom,_,define},{'(',_},{var,_,Name},{'(',_}|T]) ->
    case (catch get_define_arity(T, 0)) of
	{'EXIT', _} -> error;
	Arity       -> {define, {Name,Arity}}
    end;
fallback_method(Toks) ->
    Toks1 = sneaky(Toks),
    %% sneaky replaces ?DEF by the atom sneaky -- 
    %% and then tries to parse the result
    %% this is poor man's macro expansion
    case erl_parse:parse_form(Toks1) of
	{ok, Form} -> summarise(Form);
	{error, _} -> error
    end.

sneaky([{'?',_},{var,Ln,_}|T]) -> [{atom,Ln,'sneaky'}|sneaky(T)];
sneaky([H|T])                  -> [H|sneaky(T)];
sneaky([])                     -> []. 

get_define_arity([{var,_,_}|T], N) -> get_define_arity(T, N+1);
get_define_arity([{',',_}|T], N)   -> get_define_arity(T, N);
get_define_arity([{')',_}|_], N)   ->  N.

%% in this context whitespace is viewed as a comment

remove_whitespace([{white_space,_,_}|T]) -> remove_whitespace(T);
remove_whitespace([{comment, _,_}|T])    -> remove_whitespace(T);
remove_whitespace([H|T])                 -> [H|remove_whitespace(T)];
remove_whitespace([])                    -> [].

header() ->
    ["
<style>
pre{background-color:#ffaaee}
h2{background-color:#aaffee}
</style>
"].

pass1({Type,_,_,I}) ->
    [header(Type),"<pre>",quote(I),"</pre>"].

header(X) ->
    ["<h2>",quote(io_lib:format("~p",[X])),"</h2>"].

classify_form(Str) ->
    R = case (catch classify_form0(Str)) of
	{'EXIT', _Why} ->
	    unknown;
	Val ->
	    Val
	end,
    %% io:format("R=~p~n",[R]),
    R.
    
classify_form0(Str) ->
    case erl_scan:string(Str, 1) of
	{ok, Tokens, _} ->
	    %% io:format("Tokens=~p~n",[Tokens]),
	    summary(erl_parse:parse_form(Tokens));
	_ ->
	    oops
    end.

summary({ok,{function,_,Name,Arity,_}})  -> {function,{Name,Arity}};
summary({ok,{attribute,_,spec,{S,_}}})   -> {spec, S};
summary({ok,{attribute,_,export,L}})     -> {export,L};
summary({ok,{attribute,_,type,{S,_,_}}}) -> {type,S};
summary({ok,{attribute,_,compile,_}})    -> compile;
summary({ok, X})                         -> {funny, X};
summary(X)                               -> {bad,X}.

quote(I) ->
    elib2_misc:string2html(I).

add_line_numbers(Form) ->
    L = [I || {line, I} <- elib2_misc:deep_find(fun is_line/1, Form)],
    {lists:min(L), lists:max(L)}.
		      
is_line({line,_}) -> true;
is_line(_)        -> false.

%%----------------------------------------------------------------------
read_raw_forms(File) ->
    Str = elib2_misc:file2string(File),
    loop(erl_scan:tokens([], Str, {1,1}, [return,text]), []).

loop({done, {eof,_}, eof}, L) ->
    reverse(L);
loop({done, {ok, Toks, _}, eof}, L) ->
    reverse([Toks|L]);
loop({done, {ok, Toks, Ln}, Str1}, L) ->
    loop(erl_scan:tokens([], Str1, Ln, [return,text]), 
	 [Toks|L]);
loop({more, X}, L) ->
    loop(erl_scan:tokens(X, eof, {1,1}, [return,text]), L).

%%----------------------------------------------------------------------

normalize_toks(Toks) ->
    [normalize_tok(I) || I <- Toks].

normalize_tok(Tok) ->
    %% this is the portable way ...
    [{_,Txt}] = erl_scan:token_info(Tok, [text]),
    Txt.

%%----------------------------------------------------------------------

reduce_segments([{ws,X}|T], L) ->
    %% if it's all white_space then drop_this
    case remove_leading_ws(X) of
	[] -> reduce_segments(T, L);
	X1 -> reduce_segments(T, [{comment,reduce_comment(X1)}|L])
    end;
reduce_segments([{form1,K,V}|T], L) ->
    reduce_segments(T, [{K,V}|L]);
reduce_segments([], L) ->
    reverse(L).

remove_leading_ws([{white_space,_,_}|T]) -> remove_leading_ws(T);
remove_leading_ws(X)                     -> X.

reduce_comment(L) ->
    [reduce_comment1(I) || I <- L].

reduce_comment1({comment,_,C}) -> {str,C};
reduce_comment1({white_space,_,S}) -> {white_space,S}.
 
    
