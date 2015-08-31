%% Copyright (c) 2006-2009 Joe Armstrong
%% See MIT-LICENSE for licensing information.
%% Time-stamp: <2015-08-31 12:29:56 joearmstrong>

-module(elib2_misc).

%% elib2_misc   Miscellaneous functions
%% A large library of miscellaneous functions.  Often when I'm
%% programming I come across the situation where I wish that a
%% particular function I need were to be in the module lists or
%% string, or ... At this point I can't just go add add the routine I
%% want into lists (or whatever). These are the libraries and these
%% days it is a right old rigmarole to get them changed. If I can't
%% add my new functions to the standard libraries then where can I add
%% them? Answer: elib2_misc. elib2_misc is my dumping ground for a
%% large number of small and mostly pure functions that should
%% probably be elsewhere.
%%
%% elib2_misc is organised into sections. 

%% This is the largest section in elib2_misc the routines here are
%% in alphabetic order.

%% % -compile(export_all).

-export([
	 % mk_tree/1,
	 add_code_with_color/1,
	 add_code_with_color/2,
	 added_files/2,
	 add_sequence_number/1,
	 as_bits/1,
	 bin2hex/1,
	 bdump/2,
	 unsigned_byte_to_hex_string/1,
	 signed_byte_to_hex_string/1,
	 check_io_list/1,
	 collect_int/1,
	 collect_string/1,
	 collect_atom/1,
	 collect_word/1,
	 compression_diff/2,
	 complete/2,
	 decodeURI/1,
	 deep_find/2,
	 deep_replace/2,
	 dos2unix/1,
	 downcase_char/1,
	 dump_tmp/2, 
	 dump/2, 
	 duplicates/1,
	 encodeURI/1,
	 ensure_started/2,
	 eval_file/1,
	 eval_string/1,
	 every/3,
	 expand_tabs/1,
	 expand_env_vars/1,
	 extract_attribute/2,
	 extract_prefix/2,
	 fetch/2,
	 file2bin/1,
	 file2lines/1,
	 file2numberedlines/1,
	 file2md5/1,
	 file2sha/1,
	 file2paras/1,
	 file2stream/1,
	 file2string/1,
	 file2term/1,
	 file_size_and_type/1,
	 file_type/1,
	 find_src/1,
	 first/1, 
	 flatten_io_list/1,
	 flush_buffer/0,
	 for/3,
	 force/1,
	 foreach_chunk_in_file/3,
	 foreach_word_in_file/2,
	 foreach_word_in_string/2,
	 forever/0,
	 get_erl_section/2,
	 get_line/1,
	 get_line/2,
	 glob_dir/1,
	 hash_file/2,
	 have_common_prefix/1,
	 hex2bin/1,
	 hex2list/1,
	 hex_nibble2int/1,
	 include_dir/0,
	 include_file_name/1,
	 id/1,
	 interleave/2,
	 is_alphanum/1,
	 is_blank_line/1, 
	 is_prefix/2,
	 is_response_correct/3,
	 keep_alive/2,
	 last_modified/1,
	 lines2para/1,
	 list2frequency_distribution/1, 
	 longest_common_prefix/1,
	 lookup/2,
	 lorem/1,
	 ls/1,
	 make_challenge/0,
	 % make_global/2,
	 make_response/2,
	 make_test_strings/1,
	 make_tmp_filename/2,
	 make_module/3,
	 make_module/4,
	 merge_kv/1,
	 mini_shell/0,
	 must_ensure_dir/1,
	 must_read_file/1,
	 must_write_file/2,
	 ndots/1,
	 nibble_to_hex_char/1,
	 odd/1,
	 on_exit/2,
	 outfile/2,
	 out_of_date/2,
	 padd/2,
	 perms/1,
	 pmap/2,
	 pmap1/2,
	 priority_receive/0,
	 random_seed/0,
	 random_string/1,
	 random_string/2,
	 read_at_most_n_lines/2,
	 remove_duplicates/1,
	 remove_leading_whitespace/1,
	 remove_leading_and_trailing_whitespace/1,
	 remove_prefix/2,
	 remove_trailing_whitespace/1,
	 replace/3,
	 root_dir/0,
	 rpc/2,
	 safe/1,
	 show_loaded/1,
	 skip_blanks/1,
	 skip_to_nl/1,
	 sleep/1, 
	 spawn_monitor/3,
	 split_at_char/2,
	 split_list/2,
	 string2exprs/1,
	 string2html/1,
	 string2latex/1,
	 string2stream/1,
	 string2toks/1,
	 string2term/1,
	 string2lines/1,
	 sub_binary/3,
	 time_fun/2,
	 time_stamp/0,
	 term2file/2,
	 term2string/1,
	 test_function_over_substrings/2,
	 to_lower/1,
	 trim/1,
	 unconsult/2,
	 which/1,
	 which_added/1,
	 xml_to_iol/1
	]).

%% Templates
-export([expand_file_template/3,
	 expand_string_template/2,
	 expand_template/2,
	 file2template/1,
	 string2template/1,
	 template2file/3]).

-export([tex2pdf/1]).

-import(lists, [all/2, any/2, filter/2, reverse/1, reverse/2,
		foldl/3, foreach/2, map/2, member/2, dropwhile/2, 
		seq/2, splitwith/2, sort/1, zip/2]).

-import(filename, [dirname/1]).

-define(IN(X,Min,Max), (X >= Min andalso X =< Max)).
-define(WHITESPACE(X), ((X=:=$\s) orelse (X=:=$\t) orelse (X=:=$\r) orelse (X=:=$\n))).
-define(IS_DIGIT(X), ($0=<X andalso X=<$9)).
-define(IS_ALPHA(X), (($a=<X andalso X=<$z) orelse ($A=<X andalso X=<$Z))).

%% -define(IN(X,Min,Max), X >= Min, X =< Max).
%% -define(WHITESPACE(X),X=:=$\s;X=:=$\t;X=:=$\r;X=:=$\n).
%% -define(IS_DIGIT(X),$0=<X,X=<$9).
%% -define(IS_ALPHA(X),$a=<X,X=<$z;$A=<X,X=<$Z).

-define(obsolete(Func1, Args1, Mod, Func2, Args2),
	(io:format("called ~p:~p ~p call ~p:~p ~p instead~n",
		   [?MODULE,Func1,Args1,Mod,Func2,Args2])),
	apply(Mod, Func2, Args2)).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%% A few random test cases -- by no means complete.

%% -spec test1() -> hooray.

add_code_with_color(File) ->
    add_code_with_color(File, filename:extension(File)).

add_code_with_color(_File, _Language) ->
    nyi(add_code_with_color,2),
    "".

    
nyi(Func,Arity) ->
    io:format("elib2_misc:~p/~p has not been implemented please implement this~n",
	      [Func,Arity]). 

test1_test() ->
    ok = file:write_file("tmp",<<"abcdef">>),
    [<<"abc">>,<<"def">>] = foreach_chunk_in_file("tmp",3,fun id/1),
    [<<"abcd">>,<<"ef">>] = foreach_chunk_in_file("tmp",4,fun id/1),
    [<<"abcdef">>] = foreach_chunk_in_file("tmp",8,fun id/1),
    hooray.

%% test1_test() -> horray = test().

%%----------------------------------------------------------------------
%% @doc
%% Added files makes a listing of all the files in Dir
%% then it calls Fun(), finally in lists all the files
%% in Dir again. It returns a list of the files
%% which have been added to the directory.

-spec added_files(Dir::string(), fun(() -> T)) -> 
      {T,[File::string()]}.
 
added_files(Dir, Fun) ->
    Before = elib2_find:files(Dir, "*", true),
    Result = Fun(),
    After =  elib2_find:files(Dir, "*", true),
    Added = filter(fun(I) ->
			   not member(I, Before)
		   end, After),
    {Result, Added}.

add_sequence_number(L) when is_list(L) ->
    lists:zip(lists:seq(1, length(L)), L).

			    

%%----------------------------------------------------------------------
%% @doc convert a bitstring to a list of zeros and ones.
%% so &lt;&lt;2#11011:5>> is converted to [1,1,0,1,1]

-spec as_bits(bitstring()) -> [0|1].

as_bits(B) when is_bitstring(B) ->
    as_bits(B, bit_size(B)).

as_bits_test() -> [1,1,1,0,1,0,1] = as_bits(<<2#1110101:7>>).

as_bits(_, 0) -> [];
as_bits(<<0:1,B/bits>>, N) -> [0|as_bits(B,N-1)];
as_bits(<<1:1,B/bits>>, N) -> [1|as_bits(B,N-1)].

%%----------------------------------------------------------------------
%% @doc Convert a binary to list of hex characters.

-spec bin2hex(binary()) -> [byte()].


bin2hex(<<X:4,Y/bitstring>>) ->
    [lists:nth(X+1,"0123456789abcdef")|bin2hex(Y)];
bin2hex(<<>>) ->
    [].

bin2hex_test() -> "1234abcd" = bin2hex(<<16#1234abcd:32>>),
		  "ff" = bin2hex(<<255>>).

%%----------------------------------------------------------------------
%% @doc complete(A, L) Compute possible completions of a
%% string. A is a string, L is a list of possible strings
%% that A might be the prefix of.
%% <pre>
%% Return values:
%% error  - means no string will ever match
%% {more,L}  - means there are completions 
%%             but we need more characters. L = [Str] = a list of 
%%             possible strings that might match
%% {yes, S}  - means there is a unique completion,
%%             S  is the largest string
%%             such that A ++ S is a prefix of all the elements of L
%% </pre>

-spec complete(A::string(), L::[string()]) -> 
           error | {more, [string()]} | {yes, string()}.

complete(Str, L) ->
    case filter(fun(I) -> is_prefix(Str, I) end, L) of
	[] ->
	    error;
	[L1] ->
	    J = remove_prefix(Str, L1),
	    {yes, J};
	L1 ->
	    % L1 is not empty so it's either more or a string
	    % We know that Str is a prefix of all elements in L1
	    L2 = map(fun(I) -> remove_prefix(Str, I) end, L1),
	    % io:format("L2=~p~n",[L2]),
	    % L2 will also not be empty
	    case longest_common_prefix(L2) of
		[] ->
		    {more, L1};
		S ->
		    {yes, S}
	    end
    end.

complete_test() ->
    {yes, "bal"} = complete("ver", ["verbal","noverbal"]),
    error = complete("nice", ["verbal","noverbal"]),
    {yes,"bal"} = complete("ver", ["verbal1","verbal2"]),
    {more,["verbal","verdant"]} = complete("ver", ["verbal","verdant"]).

%%---------------------------------------------------------------------
%% @doc deep_find(Term, F) -> L
%%   applies F to every term and sub-term in T 
%%   returns a list of all matching terms

-spec deep_find(fun((B) -> boolean()), _A) -> [B].

deep_find(F, Term) ->
    deep_find1(Term, F, []).

deep_find1(X, F, L) ->
    case F(X) of
	true -> [X|L];
	false -> deep_find2(X, F, L)
    end.

deep_find2(X, F, L) when is_tuple(X) ->
    deep_find2(tuple_to_list(X), F, L);
deep_find2([H|T], F, L) ->
    L1 = deep_find1(H, F, L),
    deep_find2(T, F, L1);
deep_find2(_X, _F, L) ->
    L.

%%---------------------------------------------------------------------
%% @doc deep_replace(Term, F) -> L
%%   applies F to every term and sub-term in Term 
%%   if F(X) -> {yes, Val} then replace X by Val
%%   in the tree

-spec deep_replace(_A, fun((_B) -> {yes,_C}|no)) -> _D.

deep_replace(X, F) ->
    case F(X) of
	{yes, Val} -> Val;
	no -> deep_replace1(X, F)
    end.

deep_replace1(T, F) when is_tuple(T) ->
    L = tuple_to_list(T),
    L1 = deep_replace1(L, F),
    list_to_tuple(L1);
deep_replace1([H|T], F) ->
    H1 = deep_replace(H, F),
    T1 = deep_replace1(T, F),
    [H1|T1];
deep_replace1(X, _) ->
    X.

%% file2bin(F) -> Bin

file2bin(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

%%----------------------------------------------------------------------
%% @doc replace \r\n with \n.

-spec dos2unix(string()) -> string().

dos2unix("\r\n" ++ T) -> [$\n|dos2unix(T)];
dos2unix([H|T])       -> [H|dos2unix(T)];
dos2unix([])          -> [].

%%----------------------------------------------------------------------
%% @doc Convert uppercase character to lowercase.
    
-spec downcase_char(byte()) -> byte().
    
downcase_char(X) when $A =< X, X =< $Z -> X+ $a - $A;
downcase_char(X)                       -> X.

%%----------------------------------------------------------------------
%% @doc Lowercase a string.

-spec to_lower(string()) -> string().
    
to_lower(Str) -> map(fun downcase_char/1, Str).

to_lower_test() ->
    "abc123" = to_lower("ABC123").

%%----------------------------------------------------------------------
%% @doc
%% Dumps a term into a file. The extension .tmp
%% is added to the filename (so it is easy to delete all files
%% with the extension .tmp. This is very usuful for debugging
%% program which produce very large data structures

-spec dump(FileName::string(), Term::any()) -> _.

dump(File, Term) ->
    io:format("** dumping to ~s~n",[File]),
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p.~n",[Term]), 
    file:close(S).

%%----------------------------------------------------------------------
%% @doc
%% Dumps a term into a file. The extension .tmp
%% is added to the filename (so it is easy to delete all files
%% with the extension .tmp. This is very usuful for debugging
%% program which produce very large data structures

-spec dump_tmp(FileName::string(), Term::any()) -> _.

dump_tmp(File, Term) ->
    Out = make_tmp_filename(File, 0),
    io:format("** dumping to ~s~n",[Out]),
    {ok, S} = file:open(Out, [write]),
    io:format(S, "~p.~n",[Term]), 
    file:close(S).

make_tmp_filename(Root, N) ->
    Name = Root ++ "_" ++ integer_to_list(N) ++ ".tmp",
    case filelib:is_file(Name) of
	true -> make_tmp_filename(Root, N+1);
	false -> Name
    end.


%%----------------------------------------------------------------------
%% @doc
%% Binary dump. Dumps a term (as binary) into a file. The extension .tmp
%% is added to the filename (so it is easy to delete all files
%% with the extension .tmp. This is very usuful for debugging
%% program which produce very large data structures

-spec bdump(FileName::string(), Term::any()) -> _.

bdump(File, Term) ->
    Out = make_tmp_filename(File, 0),
    file:write_file(Out, term_to_binary(Term)).

%%----------------------------------------------------------------------
%% @doc Return a list of all duplicated items in A

-spec duplicates(A::[X]) -> B::[X].

duplicates(X) ->  duplicates(sort(X), []).

duplicates([H,H|T], [H|_]=L) -> duplicates(T, L);
duplicates([H,H|T], L)       -> duplicates(T, [H|L]);
duplicates([_|T], L)         -> duplicates(T, L);
duplicates([], L)            -> L.

%%----------------------------------------------------------------------
%% @doc
%% Ensure_started modified from 
%% % File: eunit_server.erl
%% % By Richard Carlsson &lt;richardc@it.uu.se>
%% Checks if there is a registered process called Name
%% If not spawns Fun() in a new process and registers it as Name

-spec ensure_started(Name::atom(), fun(()->_)) -> pid().
					      
ensure_started(Name, Fun) -> ensure_started(Name, Fun, 10).

ensure_started(Name, Fun, N) when N > 0 ->
    case whereis(Name) of
	undefined ->
	    Self = self(),
	    Pid = spawn(fun() -> es_server_start(Name, Self, Fun) end),
	    receive
		{Pid, ok} ->
		    Pid;
		{Pid, error} ->
		    receive after 200 -> 
			    ensure_started(Name, Fun, N - 1) 
		    end
	    end;
	Pid ->
	    Pid
    end;
ensure_started(_, _, _) ->
    throw(no_server).

es_server_start(Name, Parent, Fun) ->
    Pid = self(),
    try register(Name, Pid) of
	true ->
	    Parent ! {Pid, ok},
	    Fun()
    catch
	_:_ ->
	    Parent ! {Pid, error},
	    exit(error)
    end.

%%----------------------------------------------------------------------
%% @doc Evaluate a series of expressions contained in a file.

-spec eval_file(FileName::string()) -> any().

eval_file(File) ->
    {ok, S} = file:open(File, [read]),
    Vals = eval_file(S, 1, erl_eval:new_bindings()),
    file:close(S),
    Vals.

eval_file_test() ->
    file:write_file("./tmp.tmp",
		    "X=1,X+X.\n4*X."),
    R = eval_file("./tmp.tmp"),
    file:delete("./tmp.tmp"),
    [2,4] = R.

eval_file(S, Line, B0) ->
    case io:parse_erl_exprs(S, '', Line) of
	{ok, Form, Line1} ->
	    {value, Value, B1} = erl_eval:exprs(Form, B0),
	    [Value|eval_file(S, Line1, B1)];
	{eof, _} ->
	    []
    end.

%%----------------------------------------------------------------------
%% @doc Links to Pid then every Time Fun is
%% evaluated. If Pid exits, this process stops.

-spec every(Pid::pid(), Time::integer(), Fun::function())-> pid().

every(Pid, Time, Fun) ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  every_loop(Pid, Time, Fun)
	  end).

every_loop(Pid, Time, Fun) ->
    receive
	{'EXIT', Pid, _Why} ->
	    true
    after Time ->
	    Fun(),
	    every_loop(Pid, Time, Fun)
    end.

%%----------------------------------------------------------------------
%% @doc Expand tabs, the tab stop is assumed to be fixed 8 column tabs.

-spec expand_tabs([byte()]) -> [byte()].

expand_tabs(Xs) ->  expand_tabs(0,Xs).

expand_tabs_test() ->
    "        X" = expand_tabs("\tX"),
    "1       X" = expand_tabs("1\tX"),
    "12      X" = expand_tabs("12\tX"),
    "123     X" = expand_tabs("123\tX"),
    "1234    X" = expand_tabs("1234\tX"),
    "12345   X" = expand_tabs("12345\tX"),
    "123456  X" = expand_tabs("123456\tX"),
    "1234567 X" = expand_tabs("1234567\tX"),
    "12345678       X" = expand_tabs("12345678\tX").

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
%% @doc Extract an attribute from beam code.

-spec extract_attribute(Mod::atom(), Key::atom()) ->
			       {ok, any()} | error.

extract_attribute(Mod, Key) ->
    case beam_lib:chunks(Mod,[attributes]) of
	{ok, {Mod, [{attributes,L}]}} ->
	    lookup(Key, L);
	_ -> exit(badFile)
    end.

extract_attribute_test() ->
    {ok, _} = extract_attribute(elib2_misc,vsn).


%%----------------------------------------------------------------------
%% @doc fetch(Key, PropertyList) -> Val or raises an errors.
%% provides a wrapper around lists:keyfind

-spec fetch(K, [{K,V}]) ->  V.

fetch(Key, L) ->
    case lists:keyfind(Key, 1, L) of
	{_,Val} -> Val;
	false -> exit({eFetch,Key})
    end.

fetch_test() ->
    2 = fetch(b, [{a,1},{b,2}]).

%%----------------------------------------------------------------------
%% @doc Read file into line buffer
%% "\n" at the end of each line is retained
%% This is currently the fastest method

-spec file2lines(FileName::string()) -> [Line::string()].

file2lines(FileName) ->    
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
    after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> [Line|get_all_lines(Device)]
    end.

file2lines_test() ->
    ["%% Copyright (c) 2006-2009 Joe Armstrong\n",
     "%% See MIT-LICENSE for licensing information.\n"|_] =
	elib2_misc:file2lines("./elib2_misc.erl").


%%----------------------------------------------------------------------
%% @doc Read file into numbered line buffer. The \n is retained at the end of
%% each line.

-spec file2numberedlines(FileName::string()) -> 
   [{LineNo::integer(), Line::string()}].

file2numberedlines(File) ->    
    Lines = file2lines(File),
    zip(seq(1, length(Lines)), Lines).

file2numberedlines_test() ->
    %% test this file
    [{1,"%% Copyright (c) 2006-2009 Joe Armstrong\n"},
     {2,"%% See MIT-LICENSE for licensing information.\n"}|_] =
	file2numberedlines("./elib2_misc.erl").

%%----------------------------------------------------------------------
%% @doc compute the MD5 checksum of a file

-spec file2md5(File::string()) -> {ok, Md5::binary()} | {error, Err::any()}.

file2md5(File) ->hash_file(File, md5).

-spec file2sha(File::string()) -> {ok, Md5::binary()} | {error, Err::any()}.

file2sha(File) ->hash_file(File, sha).


hash_file(File, Method) ->
    % the file can be huge so mess with it.
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




%%----------------------------------------------------------------------
%% @doc

-spec file2paras(FileName::string()) -> 
    [{LineNumber::integer(), Para::string()}].
    
file2paras(File) ->
    Lines = file2numberedlines(File),
    file2paras_collect_paras(Lines).

file2paras_collect_paras(L) ->
    case dropwhile(fun({_, Str})  -> is_blank_line(Str) end, L) of
	[] -> 
	    [];
	L1 ->
	    {Para, L2} = splitwith(fun({_,Str}) -> 
					   not is_blank_line(Str) end, 
				   L1),
	    [file2paras_normalise_para(Para)|file2paras_collect_paras(L2)]
    end.

file2paras_normalise_para(L) ->
    Ln = element(1, hd(L)),
    Strs = flatten_io_list([S || {_,S} <- L]),
    {Ln, Strs}.

%%----------------------------------------------------------------------
%% @doc Read a file into a string.

-spec file2string(FileName::string()) -> FileContent::string().
    
file2string(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

%%----------------------------------------------------------------------
%% @doc Reads a file into a binary and 
%% does <b>binary_to_term</b> on the
%% result.

-spec file2term(FileName::string()) -> Term::any().
 
file2term(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_term(Bin).

%%----------------------------------------------------------------------
%% @doc return the file type and length in bytes 

-spec file_size_and_type(File::string()) -> 
    {dir|file, Size::integer()} | error.

file_size_and_type(File) ->
    case file:read_file_info(File) of
	{ok, Facts} ->
	    Type = case Facts#file_info.type of
		       directory -> dir;
		       regular -> file
		   end,
	    {Type, Facts#file_info.size};
	_ ->
	    error
    end.

-spec file_type(File::string()) -> 
      link|directory|regular|error.

file_type(File) ->
    case file:read_link(File) of
        {ok, _} ->
            link;
        _ ->
            case file:read_file_info(File) of
                {ok, Facts} ->
                    case Facts#file_info.type of
                        regular   -> regular;
                        directory -> directory;
                        _         -> error
                    end;
                _ ->
                    error
            end
    end.


%%----------------------------------------------------------------------
%% @doc
%% Given a module name find_src(Mod) attempts to find the source
%% code for the module. We look first in the current directory, thereafter
%% in ..\ebin. If all else fails try to load the module
%% and look in Mod:module_info(compile).

-spec find_src(Mod::atom()) -> {ok, Src::string()} | error.
	      
find_src(Mod) ->
    case code:which(Mod) of
	non_existing ->
	    error;
	BeamFile ->
	    Root = filename:rootname(BeamFile),
	    case filelib:is_file(F1 = Root ++ ".erl") of
		true ->
		    {ok, F1};
		false ->
		    %% assume it's ../src 
		    Dir = dirname(dirname(Root)),
		    F2 = Dir ++ "/src/" ++ atom_to_list(Mod) ++ ".erl",
		    case filelib:is_file(F2) of
			true ->
			    {ok, F2};
			false ->
			    %% try module_info
			    case (catch Mod:module_info(compile)) of
				{'EXIT', _} ->
				    error;
				L ->
				    case [S || {source,S} <- L] of
					[F3] ->
					    case filelib:is_file(F3) of
						true ->
						    {ok, F3};
						false ->
						    error
					    end
				    end
			    end
		    end
	    end
    end.

%%----------------------------------------------------------------------
%% @doc The analog of last(L). last(L) is the last element in the
%% list. first(L) is all the elements except the last.
%% Obeys the identity: first(X) ++ last(X) == X for noempty
%% lists X.

-spec first([X]) -> X.

first([_])   -> [];
first([H|T]) -> [H|first(T)].

%%----------------------------------------------------------------------
%% @doc flattens an I/O list.

-spec flatten_io_list(iolist()) -> [byte()].
    
flatten_io_list(L) ->
    binary_to_list(list_to_binary(L)).

%%----------------------------------------------------------------------
%% @doc Clear all message out of the process mailbox.

-spec flush_buffer() -> void.

flush_buffer() ->
    receive
	_Any ->
	    flush_buffer()
    after 0 ->
	void
    end.

%%----------------------------------------------------------------------
%% @doc for(Min, Max, F) -> [F(Min), F(Min+1), ... F(Max)].

-spec for(Min::integer(), Max::integer(), fun((integer()) -> T)) -> [T].
						  
for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I)|for(I+1, Max, F)].

%%----------------------------------------------------------------------
%% @doc Apply F(Word) to each Word contained in a given file.
%% We extract "words" from a file and call F(Word) for each word. 
%% this is done for its side effect. There is no return value.

-spec foreach_word_in_file(FileName::string(), fun((string())->any())) -> void.

foreach_word_in_file(File, F) ->
    case file:read_file(File) of
	{ok, Bin} -> foreach_word_in_string(binary_to_list(Bin), F);
	_         -> void
    end.

%%----------------------------------------------------------------------
%% @doc Apply F(Word) to each Word contained in a string.

-spec foreach_word_in_string(FileName::string(), 
			     fun((string())->any())) -> void.

foreach_word_in_string(Str, F) ->
    case collect_word(Str) of
	no -> 
	    void;
	{Word, Str1} ->
	    F(Word),
	    foreach_word_in_string(Str1, F)
    end.

%%----------------------------------------------------------------------
%% @doc
%% Open File and reads into binary chunks Len bytes at a
%% time for each chunk compute F(Bin) and return the list
%% [F(Bin)].

-spec foreach_chunk_in_file(File::string(), 
			    Len::integer(), 
                            Fun::fun((binary()) -> X)) -> [X].

foreach_chunk_in_file(File, BlockSize, Fun) ->
    Len = filelib:file_size(File),
    {ok, P} = file:open(File, [read,raw,binary]),
    %% Note numbering starts at 0 not 1
    %% there is a mistake in the book page 225
    L = foreach_process_chunks(P, 0, BlockSize, Len, Fun),
    file:close(P),
    L.

foreach_process_chunks(P, Start, Len, Max, Fun) ->
    {ok, Bin} = file:pread(P, Start, Len),
    G = Fun(Bin),
    Start1 = Start + Len,
    if
	Start1 >= Max -> [G];
	true -> [G|foreach_process_chunks(P, Start1, Len, Max, Fun)]
    end.

%%----------------------------------------------------------------------
%% @doc Suspend for a very long time.

-spec forever() -> no_return().

forever() ->
    receive
	after infinity ->
		true
	end.

%%----------------------------------------------------------------------
%%@doc
%% Extract a tagged region of a file.
%% <pre>
%% get_erl_section(File, Tag) -> Data | exit + warning message
%% </pre>
%%    Looks in File for a section marked
%%    Data is quoted for html
%% <pre>
%%   %% START:Tag
%%    ....
%%   %% END:Tag
%% </pre>
%% very strict Tag must come immediately after :

-spec get_erl_section(FileName::string(), Tag::string()) -> string().

get_erl_section(File, Tag) ->
    case file:read_file(File) of
	{ok, B} ->
	    Stuff = get_stuff(binary_to_list(B), File, Tag),
	    Stuff1 = expand_tabs(Stuff),
	    Stuff2 = string2html(Stuff1),
	    %% io:format("Stuff=~p~n",[Stuff2]),
	    Stuff2
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

%%----------------------------------------------------------------------
%% @doc get next line
%% get_line(Str) -> {Line, Str'}

-spec get_line(Buffer::string()) -> {FistLine::string(), RestBuffer::string()}.

get_line(Str) -> get_line1(Str, []).

get_line1([$\n|T], L) -> {reverse([$\n|L]), T};
get_line1([H|T], L)   -> get_line1(T, [H|L]);
get_line1([], L)      -> {reverse(L), []}.

%%----------------------------------------------------------------------
%% @doc Get line and add to reversed line.

-spec get_line(Buffer::string(),
	      ReversedLine::string()) -> 
    {FistLine::string(), RestBuffer::string()}.

get_line([$\n|T], L) -> {reverse(L), T};
get_line([H|T], L)   -> get_line(T, [H|L]);
get_line([], L)      -> {reverse(L), []}.

%%----------------------------------------------------------------------
%% @doc return the hex representation of a byte

-spec unsigned_byte_to_hex_string(integer()) -> [byte()].

%% N is in -128 .. 127    

unsigned_byte_to_hex_string(N) when N >= 0, N < 256 ->
    [nibble_to_hex_char(N bsr 4),nibble_to_hex_char(N band 15)].

unsigned_byte_to_hex_string_test() ->
    "00" = unsigned_byte_to_hex_string(0),
    "7a" = unsigned_byte_to_hex_string(122),
    "10" = unsigned_byte_to_hex_string(16),
    "ff" = unsigned_byte_to_hex_string(255). 
    
%%----------------------------------------------------------------------
%% @doc return the hex representation of a byte

-spec signed_byte_to_hex_string(integer()) -> [byte()].

%% N is in -128 .. 127    

signed_byte_to_hex_string(N) when N < 128, N > -129 ->
    <<B1:4,B2:4>> = <<N:8>>,
    [nibble_to_hex_char(B1),nibble_to_hex_char(B2)].

signed_byte_to_hex_string_test() ->
    "00" = signed_byte_to_hex_string(0),
    "7a" = signed_byte_to_hex_string(122),
    "10" = signed_byte_to_hex_string(16),
    "ff" = signed_byte_to_hex_string(-1),
    "80" = signed_byte_to_hex_string(-128).

%%----------------------------------------------------------------------
%% @doc Convert an integer in 0..15 to a hex character

-spec nibble_to_hex_char(0..15) -> byte().

nibble_to_hex_char(X) when X < 10 -> $0 + X; 
nibble_to_hex_char(X) -> $a + X - 10.

nibble_to_hex_char_test() ->
    $1 = nibble_to_hex_char(1),
    $f = nibble_to_hex_char(15).

%%----------------------------------------------------------------------
%% @doc Convert a hex string to a binary.

-spec hex2bin([char()]) -> binary().

hex2bin(Str) ->
    L = hex2list(Str),
    list_to_binary(L).

hex2bin_test() ->
    <<0,255>> = hex2bin("00ff").

%%----------------------------------------------------------------------
%% @doc Convert a hex string to a list of bytes.

-spec hex2list([char()]) -> [byte()].

hex2list([H1,H2|T]) ->
    I = hex_nibble2int(H1) * 16 + hex_nibble2int(H2),
    [I|hex2list(T)];
hex2list([]) ->
    [].

hex2list_test() ->
    [0,255] = hex2list("00ff").


%%----------------------------------------------------------------------
%% @doc Convert a hex nibble chanrcater to an integer.

-spec hex_nibble2int(char()) -> 0..15.

hex_nibble2int(X) when ?IN(X, $0, $9) -> X - $0;
hex_nibble2int(X) when ?IN(X, $a, $f) -> X - $a + 10;
hex_nibble2int(X) when ?IN(X, $A, $F) -> X - $A + 10.

hex_nibble2int_test() ->
    10 = hex_nibble2int($a),
    10 = hex_nibble2int($A),
    1  = hex_nibble2int($1).

%%----------------------------------------------------------------------
%% @doc Find the common prefix in a list of strings.

-spec have_common_prefix([string()]) -> no | {yes, byte(), [string()]}.

have_common_prefix([]) -> no;
have_common_prefix(L) ->
    case any(fun is_empty_list/1, L) of
	true  -> no;
	false ->
	    %% All lists have heads and tails
	    Heads = map(fun(I) -> hd(I) end, L),
	    H = hd(Heads),
	    case all(fun(X) -> hd(X) =:= H end, L) of
		true -> 
		    Tails = map(fun(I) -> tl(I) end, L),
		    {yes, H, Tails};
		false ->
		    no
	    end
    end.

have_common_prefix_test() ->
    {yes,$a,["bc"]} = have_common_prefix(["abc"]),
    {yes,$a,["bc","b2"]} = have_common_prefix(["abc","ab2"]),
    no = have_common_prefix(["abc","ab2","12"]).

%%----------------------------------------------------------------------
%% @doc Identity function.

-spec id(X) -> X.

id(I) -> I.

%%----------------------------------------------------------------------
%% @doc the root dir of the library

-spec root_dir() -> Directory::string().

root_dir() ->
    d(d(d(code:which(?MODULE)))).

%%----------------------------------------------------------------------
%% @doc Return the name of the include directory.

-spec include_dir() -> DirName::string().
    
include_dir() ->
    filename:join(root_dir(), "include").

%%----------------------------------------------------------------------
%% @doc Find the fully qualified name of a file in the include directory.

-spec include_file_name(FileName::string()) ->
    FullyQualifiedFilename::string().

include_file_name(X) ->
    filename:join(include_dir(), X).

%%----------------------------------------------------------------------
%% @doc Same as filename:dirname

-spec d(string()) -> string().
    
d(X) ->
    filename:dirname(X).

%%----------------------------------------------------------------------
%% @doc Interleave a symbol Sep between elements in a list.
%% Given a list [X1,X2,...,Xn] makes the list.
%% [X1,Sep,X2,Sep,...,Sep,Xn].

-spec interleave(Sep::X, In::[X]) -> Out::[X].

interleave(_, [X]) -> [X];
interleave(_, []) -> [];
interleave(Sep, [H|T]) -> [H,Sep|interleave(Sep, T)].

%%----------------------------------------------------------------------
%% @doc check if a line is all blank

-spec is_blank_line(string()) -> boolean().

is_blank_line([$\s|T]) -> is_blank_line(T);
is_blank_line([$\n|T]) -> is_blank_line(T);
is_blank_line([$\r|T]) -> is_blank_line(T);
is_blank_line([$\t|T]) -> is_blank_line(T);
is_blank_line([]) -> true;
is_blank_line(_)  -> false.

%%----------------------------------------------------------------------
%% @doc test is an object is a list and is empty.

-spec is_empty_list([_X]) -> boolean().

is_empty_list([]) ->	true;
is_empty_list(X) when is_list(X) -> false.

%%----------------------------------------------------------------------
%% @doc Test if an I/O list really is an IO list.

-spec check_io_list(iolist()) -> true.

check_io_list([H|T]) ->
    check_io_list(H) and check_io_list(T) and good_tail(T);
check_io_list(X) when is_integer(X), X >= 0, X =< 255 ->
    true;
check_io_list(B) when is_binary(B) ->
    true;
check_io_list([]) ->
    true;
check_io_list(X) ->
    throw({badElementInIoList, X}).

good_tail(L) when is_list(L) -> true;
good_tail(_) -> false.
    

%%----------------------------------------------------------------------
%% @doc true if A is a prefix of B otherwise false.

-spec is_prefix(A::[X], B::[X]) -> boolean().

is_prefix([], _)         -> true;
is_prefix([H|T], [H|T1]) -> is_prefix(T, T1);
is_prefix(_, _)          -> false.

is_prefix_test() ->
    true = is_prefix("ab", "abcdef"),
    false = is_prefix("ab", "12abcdef"),
    false = is_prefix("ab", "a1bcdef").

%%----------------------------------------------------------------------
%% @doc if A is a prefix of B return {yes, C} where A ++ C = B 
%% otherwise no

-spec extract_prefix(A::[X], B::[X]) -> {yes, Rest::[X]} | no.

extract_prefix([], B)         -> {yes, B};
extract_prefix([H|T], [H|T1]) -> extract_prefix(T, T1);
extract_prefix(_, _)          -> no.

%%----------------------------------------------------------------------
%% @doc used in an MD5 callenge response test

-spec is_response_correct(Challenge::string(), Response::binary(),
			  Secret::string()) -> boolean().

is_response_correct(Challenge, Response, Secret) ->
    case erlang:md5(Challenge ++ Secret) of
        Response -> true;
        _        -> false
    end.
%%----------------------------------------------------------------------
%% @doc Makwe the response for a challenge response authentication
%% algorithm.

-spec make_response(Challenge::string(), Secret::string()) -> 
    Response::binary().

make_response(Challenge, Secret) ->
    erlang:md5(Challenge ++ Secret).

make_response_test() ->
    Response = make_response("hello", "1234"),
    true = is_response_correct("hello", Response, "1234"),
    false = is_response_correct("hello", <<"123">>, "1234").

%%----------------------------------------------------------------------
%% @doc Keep a registerd process alive, restarting if necessary.

-spec keep_alive(Name::atom(), fun(() -> _X)) -> any().
					  
keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

%%----------------------------------------------------------------------
%% @doc Make a frequency distribution of a numbner of items in a list.

-spec list2frequency_distribution([X]) ->
    [{X, integer()}].

list2frequency_distribution(L) ->
    D1 = lists:foldl(fun list2frequency_distribution/2, dict:new(), L),
    dict:to_list(D1).

list2frequency_distribution_test() ->
    L = list2frequency_distribution([a,b,c,a,b,a,a,b]),
    [] = L -- [{a,4},{b,3},{c,1}].

list2frequency_distribution(H, D) ->
    case dict:find(H, D) of
	{ok, N} -> dict:store(H, N+1, D);
	error   -> dict:store(H, 1, D)
    end.

%%----------------------------------------------------------------------
%% @doc Add \n between lines with \n's.

-spec lines2para([string()]) -> string().

lines2para(Lines) ->
    flatten_io_list(interleave($\n, Lines)).

%%----------------------------------------------------------------------
%% @doc Given a list of strings return the longest prefix which is
%% common to all lists.

-spec longest_common_prefix([string()]) -> string().

longest_common_prefix(L) ->
    longest_common_prefix(L, []).

longest_common_prefix(Ls, L) ->
    case have_common_prefix(Ls) of
	{yes, H, Ls1} ->
	    longest_common_prefix(Ls1, [H|L]);
	no ->
	    reverse(L)
    end.

longest_common_prefix_test() ->
    [] = longest_common_prefix(["abc","def"]),
    "abc" = longest_common_prefix(["abc","abcdef"]),
    [] = longest_common_prefix(["abc","abcdef","12"]),
    "ab" = longest_common_prefix(["abc","abcdef","ab12"]).

%%----------------------------------------------------------------------
%% @doc Fast property list lookup.
%% Fast because it uses the internal lists:keysearch which is a BIF

-spec lookup(K, [{K,V}]) -> {ok, V} | error.
    
lookup(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	{value, T} -> {ok, element(2, T)};
	false -> error
    end.

%%----------------------------------------------------------------------
%% @doc List the files in a directory.
%% Returns a directory listing <b>relative to Dir</b> -- the filenames are sorted.
%% classified as files or directories and the sizes are given.

-spec ls(Dir::string()) -> [{FileName::string(), file|dir, Size::integer()}].

ls(Dir) ->
    case file:list_dir(Dir) of
	{ok, L} ->
	    foldl(fun(I, A) ->
			  F1 = filename:join(Dir, I),
			  case file_size_and_type(F1) of
			      {Type, Size} ->
				  [{I, Type, Size}|A];
			      error ->
				  A
			  end
		  end, [], sort(L));
	{error, _} ->
	    exit({eLsDirectoryDoesNotExist, Dir})
    end.

%%----------------------------------------------------------------------
%% @doc Make a random string to be used as a challenge in 
%% challenge response authentication.

-spec make_challenge() -> string().

make_challenge() ->
    random_string(25).

%%----------------------------------------------------------------------
%% @doc make_test_strings(Str)

-spec make_test_strings(string()) -> [string()].

make_test_strings(Str) ->
    L = length(Str),
    make_test_strings(Str, L+1, 1).

make_test_strings_test() ->
    ["a","ab","abc","abcd","abcde","abcdef"] = make_test_strings("abcdef").

make_test_strings(_, Max, Max) -> [];
make_test_strings(Str, Max, N) ->
    [string:sub_string(Str, 1, N)|make_test_strings(Str, Max, N+1)].

%%----------------------------------------------------------------------
%% @doc
%% Take a property list [{Key,Val}] where Key can occur
%%    More than once and make it into a list {Key, [Val]} where
%%    each Key occurs only once.

-spec merge_kv(Kv::[{Key,Val}]) -> Kv1::[{Key,[Val]}].

merge_kv(KV) ->  merge_kv(KV, dict:new()).

merge_kv([{Key,Val}|T], D0) ->
    case dict:find(Key, D0) of
	{ok, L} -> merge_kv(T, dict:store(Key, [Val|L], D0));
	error   -> merge_kv(T, dict:store(Key, [Val], D0))
    end;
merge_kv([], D) ->
    dict:to_list(D).

merge_kv_test() ->
    L = merge_kv([{a,1},{a,3},{b,4}]),
    2 = length(L),
    true = member({a, [3,1]}, L),
    true = member({b, [4]}, L),
    L1 = merge_kv([{a,1},{b,2},{a,3}]),
    2 = length(L1),
    true = member({a, [3,1]}, L1),
    true = member({b, [2]}, L1).

%%----------------------------------------------------------------------
%% @doc Emulate the shell.

-spec mini_shell() -> void.

mini_shell() ->
    mini_shell(erl_eval:new_bindings()).

mini_shell(Bindings0) ->
    case io:get_line('>>> ') of
	"q\n" -> void;
	Str ->
	    {Value, Bindings1} = eval_string(Str, Bindings0),
	    io:format("~p~n",[Value]),
	    mini_shell(Bindings1)
    end.



%%----------------------------------------------------------------------
%% @doc Count the number of dots in a string

-spec ndots(string()) -> integer().

ndots([$.|T]) -> 1 + ndots(T);
ndots([_|T])  -> ndots(T);
ndots([])     -> 0.

%%----------------------------------------------------------------------
%% @doc odd(X) is true if X is odd

-spec odd(X::integer()) -> boolean().

odd(X) ->
    case X band 1 of
	1 -> true;
	0 -> false
    end.

%%----------------------------------------------------------------------
%% @doc on_exit(Pid, Fun) links to Pid. If Pid dies
%% with reason Why then Fun(Why) is evaluated.

-spec on_exit(pid(), fun((_) -> Y)) -> Y.
    
on_exit(Pid, Fun) ->
    spawn(fun() -> 
		  process_flag(trap_exit, true),
		  link(Pid),                    
		  receive
		      {'EXIT', Pid, Why} ->     
			  Fun(Why) 
		  end
	  end).

%%----------------------------------------------------------------------
%% @doc Change the extension of FileName to Ext.

-spec outfile(File::string(), Extension::string()) -> File1::string().
    
outfile(File, Ext) ->
    filename:rootname(File) ++ Ext.

%%----------------------------------------------------------------------
%% @doc true if last_modification_time(Dest) > last_modification_time(Src)
%% otherwise false.

-spec out_of_date(Src::string(), Dest::string()) -> boolean().
%% raises no_input

out_of_date(In, Out) ->
    case filelib:is_file(In) of
	true ->
	    case filelib:is_file(Out) of
		true ->
		    %% check the time stamps
		    Tsrc  = filelib:last_modified(In),
		    Tdest = filelib:last_modified(Out),
		    if Tsrc > Tdest -> true;
		       true         -> false
		    end;
		false ->
		    %% no output so we have to recompile
		    true
	    end;
	false ->
	    %% error input cannot be found
	    %% not sure why this would be called
	    exit({out_of_date,no_input,In})
    end.


%%----------------------------------------------------------------------
%% @doc
%% priority receive

-spec priority_receive() -> {alarm, any()} | any().

priority_receive() ->
    receive
	{alarm, X} ->
	    {alarm, X}
    after 0 ->
	receive
	    Any ->
		Any
	end
    end.

%%----------------------------------------------------------------------
%% @doc Seed the random number generator.
%% This is evaluated for its side effect. Not well tested at all.

-spec random_seed() -> void.

random_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}),
    void.

%%----------------------------------------------------------------------
%% @doc Read the first N lines of a file.

-spec read_at_most_n_lines(FileName::string(), N::integer()) ->
    [Line::string()].

read_at_most_n_lines(File, N) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    split_into_lines1(binary_to_list(Bin), N,  []);
	{error, _} ->
	    exit(eNoFile)
    end.
 
read_at_most_n_lines_test() ->
    ["%% Copyright (c) 2006-2009 Joe Armstrong\n",
     "%% See MIT-LICENSE for licensing information.\n"] =
	read_at_most_n_lines("./elib2_misc.erl", 2).

%%----------------------------------------------------------------------
%% @doc Remove duplicate entries from a list. The resulting list
%% is sorted.

-spec remove_duplicates([X]) -> [X].

remove_duplicates(L) ->
    remove_duplicates(lists:sort(L), []).

remove_duplicates([H|X=[H|_]], L) -> remove_duplicates(X, L);
remove_duplicates([H|T], L)       -> remove_duplicates(T, [H|L]);
remove_duplicates([], L)          -> reverse(L).

remove_duplicates_test() ->
    [a,b,c] = remove_duplicates([b,b,a,c,a,b,c,a,b,b,a]).

%%----------------------------------------------------------------------
%% @doc remove leading and trailing white space from a string.

-spec trim(string()) -> string().
    
trim(S) ->
    remove_leading_and_trailing_whitespace(S).

trim_test() ->
    "abc" = trim("    abc   ").

%%----------------------------------------------------------------------
%% @doc remove leading and trailing white space from a string.

-spec remove_leading_and_trailing_whitespace(string()) -> string().
    
remove_leading_and_trailing_whitespace(X) -> 
    remove_leading_whitespace(remove_trailing_whitespace(X)).

remove_leading_and_trailing_whitespace_test() ->
    "abc" = remove_leading_and_trailing_whitespace("\r\t  \n \s  abc").

%%----------------------------------------------------------------------
%% @doc remove leading white space from a string.

-spec remove_leading_whitespace(string()) -> string().

remove_leading_whitespace([$\n|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\r|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\s|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace([$\t|T]) -> remove_leading_whitespace(T);
remove_leading_whitespace(X) -> X.

%%----------------------------------------------------------------------
%% @doc remove_prefix(X, Y) -> Z finds Z such that X ++ Z = Y
%% Note: this is only called when it is known that 
%% X is a prefix of Y

-spec remove_prefix(X::[A], Y::[A]) -> Z::[A].

remove_prefix([H|T], [H|T1]) -> remove_prefix(T, T1);
remove_prefix([], L)         -> L.

remove_prefix_test() ->
    "def" = remove_prefix("abc", "abcdef").

%%----------------------------------------------------------------------
%% @doc remove trailing white space from a string.

-spec remove_trailing_whitespace(string()) -> string().

remove_trailing_whitespace(X) ->
    reverse(remove_leading_whitespace(reverse(X))).

%%----------------------------------------------------------------------
%% @doc
%% Replace and Key with Key,Val in the property list 
%% Old.

-spec replace(Key, Val, Old::[{Key,Val}]) -> [{Key,Val}].

replace(Key, Val, Old) ->
    replace(Key, Val, Old, []).

replace(Key, Val1, [{Key,_Val}|T], L) ->
    reverse(L, [{Key, Val1}|T]);
replace(Key, Val, [H|T], L) ->
    replace(Key, Val, T, [H|L]);
replace(Key, Val, [], L) ->
    [{Key,Val}|L].

%%----------------------------------------------------------------------
%% @doc Standard RPC.

-spec rpc(pid(), any()) -> any().
		 
rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
	{Pid, Reply} ->
	    Reply
    end.

%%----------------------------------------------------------------------
%% @doc Evauate a function within a catch to make it safe.

-spec safe((fun(() -> X)))
 -> {error, any()} | X.
    
safe(Fun) ->
    case (catch Fun()) of
	{'EXIT', Why} -> {error, Why};
	Other         -> Other
    end.

%%----------------------------------------------------------------------
%% @doc
%% show_loaded(Fun0) -> Mods
%%   show which functions get loaded
%%   when you evaluate Fun0().

-spec show_loaded(fun( () -> _)) -> [Mod::atom()].
			     

show_loaded(Fun0) ->
    Mods1 = [Mod || {Mod,_} <- code:all_loaded()],
    Fun0(),
    Mods2 = [Mod || {Mod,_} <- code:all_loaded()],
    Mods2 -- Mods1.

%%----------------------------------------------------------------------
%% @doc Remove any spaces at the beginning of a string.

-spec skip_blanks(string()) -> string().

skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(X)       -> X.

skip_blanks_test() ->
    "abc" = skip_blanks("   abc").

%%----------------------------------------------------------------------
%% @doc sleep(N) sleeps for N milliseconds.

-spec sleep(TimeInMilliseconds::integer()) -> true.

sleep(T) ->
    receive
    after T ->
       true
    end.

%%----------------------------------------------------------------------
%% @doc spawn_monitor behaves just like spawn but
%% prints a message when the function terminates.
%% spawn_monitor(Term, Flag, Fun0) spawns Fun0 and prints Term
%% when the function terminates if Flag =0 true.

-spec spawn_monitor(Term::any(), Flag::boolean(),
		    fun(() -> any())) -> pid().

spawn_monitor(_, false, Fun) ->
    spawn(Fun);
spawn_monitor(Term, true, Fun) ->
    spawn(fun() -> spawn_monitor_starter(Term, Fun) end).

spawn_monitor_starter(Term, Fun) ->
    S = self(),
    io:format("process:~p started at:~p ~p~n",
	      [self(), erlang:now(), Term]),
    Monitor = spawn_link(fun() -> spawn_monitor1(Term, S) end),
    receive
	{Monitor, ready} ->
	    Fun()
    end.

spawn_monitor1(Term, Parent) ->
    process_flag(trap_exit, true),
    Parent ! {self(), ready},
    receive
	{'EXIT', Parent, Why} ->
	    io:format("process:~p dies at:~p ~p reason:~p~n",
		      [self(), erlang:now(), Term, Why])
    end.

%%----------------------------------------------------------------------
%% @doc
%% split(Fun, [X]) -> [{Header,[X]}]
%% Splits a lists
%% Fun(X) -> {yes, Header} | no
%% the list L is partitioned into segments 
%% for which Fun(X) -> {yes,Header}
%% The header '$none' is used for leading
%% elements in the list

-spec split_list(fun((X) -> {yes,Header} | no), [X]) ->
    [{Header,[X]}].

split_list(Fun1, L) ->
    split_list(L, Fun1, '$none', [], []).

split_list_test() ->
    F = fun("?tag=" ++ T) -> {yes,T};
	   (_) -> no
	end,
    [{"one",["a","b"]},{"two",["1","2"]}] = 
	split_list(F, ["?tag=one","a","b","?tag=two","1","2"]).

split_list([], _F, _Header, [], L) ->
    reverse(L);
split_list([], _F, Header, Content, L) ->
    Block = {Header, reverse(Content)}, 
    reverse([Block|L]);
split_list([H|T], F, Header, Content, L) ->
    case F(H) of
	{yes, Header1} ->
	    case Content of
		[] ->
		    split_list(T, F, Header1, [], L);
		_ ->
		    Block = {Header, reverse(Content)}, 
		    split_list(T, F, Header1, [], [Block|L])
	    end;
	no ->
	    split_list(T, F, Header, [H|Content], L)
    end.

%%----------------------------------------------------------------------
%% @doc Split a string at the first occurance of a given character.

-spec split_at_char(string(), char()) -> 
    {yes, Before::string(), After::string()} | no.

split_at_char(Str, C) -> split_at_char(Str, C, []).

split_at_char_test() ->
    {yes, "abc","def"} = split_at_char("abc+def", $+),
    no = split_at_char("abc+def", $-).

split_at_char([C|T], C, L) -> {yes, reverse(L), T};
split_at_char([H|T], C, L) -> split_at_char(T, C, [H|L]);
split_at_char([], _, _)    -> no.

%%----------------------------------------------------------------------
%% @doc Convert a string containing a number of expressions to 
%% the abstract form of the expressions.

-spec string2exprs(Str::string()) -> {ok, _Exprs} | error.

%% _Exprs does have a type but it's a wee bit compilcated

string2exprs(Str) ->
    case erl_scan:tokens([], Str ++ ". ", 1) of
	{done, {ok, Toks, _}, []} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok, Exprs} ->
		    {ok, Exprs};
		{error,{Line,Mod,Arg}} ->
		    EStr = io_lib:format("~s",[apply(Mod,format_error,[Arg])]),
		    Msg = lists:flatten(EStr),
		    io:format("~n***PARSE ERROR in line:~w ~s~n", [Line,Msg]),
		    io:format("Str=~s~n",[Str]),
		    error
	    end;
	Other ->
	    io:format("~n***SCAN ERROR:~p~n", [Other]),
	    error
    end.

string2exprs_test() ->
    {ok,[{match,1,{var,1,'X'},{integer,1,1}},
	 {op,1,'*',{var,1,'X'},{var,1,'X'}}]}
	= string2exprs("X=1,X*X").

%%----------------------------------------------------------------------
%% @doc Quote &lt; and &amp; so they can be included in HTML.

-spec string2html(string()) -> string().

string2html("<" ++ T) -> "&lt;" ++ string2html(T);
string2html("&" ++ T) -> "&amp;" ++ string2html(T);
string2html([H|T])    -> [H|string2html(T)];
string2html([])       -> [].
    
%%----------------------------------------------------------------------
%% @doc Turn a Erlang string into a LateX string quoting where necessary.

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
string2latex([H|T])        -> [H|string2latex(T)];
string2latex([])           -> [].

%%----------------------------------------------------------------------
%% @doc Convert a string to a term.  

-spec string2term(string()) -> any().
    
string2term(Str) ->
    {ok,Tokens,_} = erl_scan:string(Str ++ "."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

string2term_test() ->
    {abc,123} = string2term("{abc,123}").

%%----------------------------------------------------------------------
%% @doc simple tokeniser. Turns a string into a list of tokens

-spec string2toks(string()) ->
    [{comment,string()} | {int,integer()} |
     {str,string()}|{atom,string()}|{punct,string()}].

string2toks([$%|T]) ->
    {Comment,T1} = collect_comment(T, []),
    [{comment,Comment}|string2toks(T1)];	     
string2toks([H|_]=L) when ?IS_DIGIT(H) -> 
    {Int, T1} = collect_int(L),				   
    [{int,Int}|string2toks(T1)];
string2toks([H|T]) when ?WHITESPACE(H) ->
    string2toks(T);
string2toks([X|_]=L) when ?IS_ALPHA(X) ->
    {Name, T1} = collect_atom(L),				   
    [{atom,Name}|string2toks(T1)];
string2toks([$"|T]) ->
    {Str, T1} = collect_string(T, $", []),				   
    [{str,Str}|string2toks(T1)];
string2toks([$'|T]) ->
    {Str, T1} = collect_string(T, $', []),				   
    [{str,Str}|string2toks(T1)];
string2toks([H|T]) ->
    [{punct,[H]}|string2toks(T)];
string2toks([]) ->
    [].

string2toks_test() ->
    [{atom,"abc"},
     {punct,","},
     {int,123},
     {str,"abc"},
     {punct,"{"},
     {punct,","},
     {int,12},
     {punct,"."},
     {int,23}] = elib2_misc:string2toks("abc,123'abc'{,12.23").

%%----------------------------------------------------------------------
%% @doc



collect_int_test() ->
    {123,"def"} = collect_int("123def").

%%----------------------------------------------------------------------
%% @doc extract a base ten integer from the start of a string

-spec collect_int(string()) -> {integer(), string()}.
    
collect_int([H|_]=L)  when ?IS_DIGIT(H) -> collect_int(L, 0).

collect_int([H|T], N) when ?IS_DIGIT(H) -> collect_int(T, N*10 + H - $0);
collect_int(L, N) -> {N, L}.


%%----------------------------------------------------------------------
%% @doc test if a character is aphanumeric.

-spec is_alphanum(byte()) -> boolean().

is_alphanum(X)  when ?IS_ALPHA(X) ;  ?IS_DIGIT(X) -> true;
is_alphanum(_) -> false.

%%----------------------------------------------------------------------
%% @doc Extract an atom from the start of a string.

-spec collect_atom(Str::string()) -> 
    {Atom::string(), Str1::string()}.

collect_atom(L) -> collect_atom(L, []).

collect_atom_test() ->
    {"abc129","+23"} = collect_atom("abc129+23").

collect_atom([H|T], L) when ?IS_ALPHA(H) ; ?IS_DIGIT(H) ->
    collect_atom(T, [H|L]);
collect_atom([$:|T], L) ->
    collect_atom(T, [$:|L]);
collect_atom([$_|T], L) ->
    collect_atom(T, [$_|L]);
collect_atom(T, L) ->
    {reverse(L), T}.

%%----------------------------------------------------------------------
%% @doc

collect_comment("\r\n" ++ T, L) -> {reverse(L), T};
collect_comment("\n" ++ T, L)   -> {reverse(L), T};
collect_comment([], L)          -> {reverse(L), []};
collect_comment([H|T], L)       -> collect_comment(T, [H|L]).
    
%%----------------------------------------------------------------------%
%% @doc collect_string(S) -> {S1, S2} such that
%% S = S1 ++ S2. Here s1 must begin with a single or double quote.

-spec collect_string(string()) -> {string(), string()}.

collect_string([$"|T]) -> collect_string(T, $", []);
collect_string([$'|T]) -> collect_string(T, $', []).

collect_string_test() ->
    {"abc"," def"} = elib2_misc:collect_string("\"abc\" def"),
    {"abc"," def"} = elib2_misc:collect_string("\'abc\' def").

collect_string([H|T], H, L)    -> {reverse(L), T};
collect_string([], _, L)       -> throw({eof_in_string,L});
collect_string([H|T], Stop, L) -> collect_string(T, Stop, [H|L]).

%%----------------------------------------------------------------------
%% @doc Evaluate code in a string

-spec eval_string(string()) -> any().

eval_string(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.

eval_string_test() ->
    2 = eval_string("1+1"),
    4 = eval_string("X=2,X+X").
    
eval_string(Str, Bindings0) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, Bindings1} = erl_eval:exprs(Exprs, Bindings0),
    {Value, Bindings1}.


%%----------------------------------------------------------------------
%% @doc return the binary between Start and Stop in Bin

-spec sub_binary(binary(), Start::integer(), Stop::integer()) ->
    binary().

sub_binary(Bin, Start, Stop) ->
    N   = Start-1,
    Len = Stop- Start + 1,
    %% io:format("N=~p Len=~p size=~p~n",[N,Len,size(Bin)]),
    <<_:N/binary,Segment:Len/binary,_/binary>> = Bin,
    Segment.

%%----------------------------------------------------------------------
%% @doc
%% fill in the template and write to file

-spec template2file([binary()|string()],
		    [{string(), string()}], FileName::string()) ->
    ok.

template2file(Template, Assoc, FileOut) ->
    Content = expand_template(Template, Assoc),
    file:write_file(FileOut, Content).

%%----------------------------------------------------------------------

split_into_lines1([], _, L) ->
    reverse(L);
split_into_lines1(_, 0, L) ->
    reverse(L);
split_into_lines1(Str, Ln, L) ->
    {Line, Rest} = get_line(Str),
    split_into_lines1(Rest, Ln-1, [Line|L]).
    
%%----------------------------------------------------------------------
%% @doc
%% true if Infile was modified after OutFile.

last_modified(File) ->
    case file:read_file_info(File) of
        {ok, Info} ->
            Info#file_info.mtime;
        _ ->
            0
    end.

%%----------------------------------------------------------------------
%% @doc
%% Split a string into lines. Each line is terminated by
%% \n of eof. \n at end of each line is retained

-spec string2lines(string()) -> [string()].

string2lines(Str) -> string2lines(Str, []).
 
string2lines_test() ->
    ["abc\n",
     "def\n",
     "123"] = string2lines("abc\ndef\n123").  

string2lines("\n" ++ Str, Acc) -> [reverse([$\n|Acc]) | string2lines(Str,[])];
string2lines([H|T], Acc)       -> string2lines(T, [H|Acc]);
string2lines([], [])           -> [];
string2lines([], Acc)          -> [reverse(Acc)].

%%----------------------------------------------------------------------
%% @doc Write term_to_binary(Term) to a file.

-spec term2file(FileName::string(), Term::any()) ->
    ok.

term2file(File, Term) ->
    file:write_file(File, term_to_binary(Term)).

%%----------------------------------------------------------------------
%% @doc Convert a term to a printable string.

-spec term2string(any()) -> PrintableString::string().
    
term2string(Term) ->
    lists:flatten(io_lib:format("~p",[Term])).

%%----------------------------------------------------------------------
%% @doc Tests a function by calling it with different substrings of
%% a given string.

-spec test_function_over_substrings(fun(() -> any()), string()) -> any().

test_function_over_substrings(F, Str) ->
    L = make_test_strings(Str),
    foreach(fun(S) ->
		    io:format("|~s|~n    => ~p~n", [S, F(S)])
	    end, L).

%%----------------------------------------------------------------------
%% @doc Write a file (inverse of consult).
%% A joke.

-spec unconsult(FileName::string(), Term::any()) ->
		       ok.

unconsult(File, L) ->
    {ok, S} = file:open(File, [write]),
    lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
    file:close(S).

%%----------------------------------------------------------------------
%% @doc Return all permutations of a string.

-spec perms(string()) -> [string()].

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

perms_test() ->
    X = perms("abc"),
    [] = X -- ["abc","acb","bac","bca", "cab", "cba"].
 
%%----------------------------------------------------------------------
%% @doc
%% Parallel version of map

-spec pmap(fun((X) -> Y), [X]) -> [Y].    

pmap(F, L) -> 
    S = self(),
    %% % make_ref() returns a unique reference
    %% %  we'll match on this later
    Ref = erlang:make_ref(), 
    Pids = map(fun(I) -> 
		       spawn(fun() -> do_f(S, Ref, F, I) end)
	       end, L),
    %% % gather the results
    gather(Pids, Ref).

pmap_test() ->
    [1,4,9,16] = pmap(fun(I) -> I*I end, [1,2,3,4]).

do_f(Parent, Ref, F, I) ->					    
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
    receive
	{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].


 
%%----------------------------------------------------------------------
%% @doc
%% Parallel map that is not order preserving. 
%% pmap1([X,Y,Z]) return any permutation of  [F(X), F(Y), F(Z)].

-spec pmap1(fun((X) -> Y), [X]) -> [Y].    

pmap1(F, L) -> 
    S = self(),
    Ref = erlang:make_ref(),
    foreach(fun(I) -> 
		    spawn(fun() -> do_f1(S, Ref, F, I) end)
	    end, L),
    %% % gather the results
    gather1(length(L), Ref, []).

pmap1_test() ->
    L = pmap1(fun(I) -> I*I end, [1,2,3,4]),
    [] = L -- [1,4,9,16]. 

do_f1(Parent, Ref, F, I) ->					    
    Parent ! {Ref, (catch F(I))}.

gather1(0, _, L) -> L;
gather1(N, Ref, L) ->
    receive
	{Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
    end.

%%----------------------------------------------------------------------
%% @doc Test if a character belongs to a word.

-spec is_word_char(byte()) -> boolean().

is_word_char(X) when $A=< X, X=<$Z -> true;
is_word_char(X) when $0=< X, X=<$9 -> true;
is_word_char(X) when $a=< X, X=<$z -> true;
is_word_char(_)  -> false.

%%----------------------------------------------------------------------
%% @doc Extracts the next word from the start of a string.

-spec collect_word(Str::string()) -> 
    {Word::string(), Str1::string()} | no.

collect_word([H|T]) ->
    case is_word_char(H) of
	true  -> collect_word(T, [H]);
	false -> collect_word(T)
    end;
collect_word([]) ->
    no.

collect_word([H|T]=All, L) ->
    case is_word_char(H) of
	true  -> collect_word(T, [H|L]);
	false -> {reverse(L), All}
    end;
collect_word([], L) ->
    {reverse(L), []}.

%%----------------------------------------------------------------------
%% @doc computes the normalised compression difference between
%% two binaries

compression_diff(X, Y) when is_binary(X), is_binary(Y) ->
    Zx = size(term_to_binary(X, [compressed])),
    Zy = size(term_to_binary(Y, [compressed])),
    Zxy  = size(term_to_binary(<<X/binary,Y/binary>>, [compressed])),
    (Zxy - lists:min([Zx, Zy]))/lists:max([Zx,Zy]).


%%----------------------------------------------------------------------
%% @doc
%% add X to L if it is not a member of the list
%% padd is short for "possible add"

-spec padd(X, L::[X]) -> L1::[X].
  
padd(X, L) ->
    case member(X, L) of
	true -> L;
	false -> [X|L]
    end.

%%----------------------------------------------------------------------
%% @doc
%% Return a random string with exactly N characters.

-spec random_string(N::integer()) -> string().

random_string(N) -> random_seed(), random_string1(N, []).

random_string1(0, D) -> D;
random_string1(N, D) ->
    random_string1(N-1, [random:uniform(26)-1+$a|D]).

%%----------------------------------------------------------------------
%% @doc
%% Return a random string with between N1 and N2 characters.

-spec random_string(Min::integer(), Max::integer()) -> string().

random_string(Min, Max) when Max > Min -> 
    random_seed(),
    Len = Min + random:uniform(Max-Min),
    random_string1(Len, []).

%%----------------------------------------------------------------------
%% @doc Find the directory name where a module was loaded from.

-spec which(Mod::atom()) -> Directory::string().
    
which(Mod) ->
    filename:dirname(code:which(Mod)).

%%----------------------------------------------------------------------
%% @doc time_fun(Str, Fun)
%%    times Fun().
%%    Prints Str and time if evaluation of Fun succeeds
%%    otherwise prints error

-spec time_fun(Description::string(), fun(() -> T)) -> T.
						 
time_fun(Str, Fun) ->
    case timer:tc(?MODULE, force, [Fun]) of
	{_Time, {'EXIT', Why}} ->
	    io:format("Timeing:~s EXIT:~p~n",[Str, Why]);
	{Time, Result} ->
	    io:format("~s Took ~w ms.~n",[Str, Time div 1000]),
	    Result
    end.

%%----------------------------------------------------------------------
%% @doc Force evauation of a fun

-spec force(fun(() -> T)) -> T.

force(Fun) -> Fun().

%%----------------------------------------------------------------------
%% @doc return a string containing the current data and time

-spec time_stamp() -> string().
    
time_stamp() ->
    {Year,Month,Day} = date(),
    Year1 = Year - 2000,
    {Hour,Min,Sec} = time(),
    lists:flatten(io_lib:format("~2.2.0w~2.2.0w~2.2.0w_~2.2.0w~2.2.0w~2.2.0w",
				[Year1,Month,Day,Hour,Min,Sec])).

%%----------------------------------------------------------------------
%% @doc
%% spawn a fun and find out which additional modules get loaded.
%% Returns the names of the new modules which have been loaded and 
%% where they were loaded from.

-spec which_added( fun(() -> any())) -> 
    [{ModuleName::atom(), DirName::string()}].

which_added(Fun) ->
    In = [Mod || {Mod,_} <- code:all_loaded()],
    spawn(fun() -> Fun() end),
    sleep(1000),
    Out = [Mod || {Mod,_} <- code:all_loaded()],
    New = Out -- In,
    [{I,which(I)} || I <- New].

%%----------------------------------------------------------------------
%% @doc

get_val(Key, [{Key,Val}|_]) -> Val;
get_val(Key, [_|T])         -> get_val(Key, T);
get_val(_, [])              -> [].

%% Templates
%% =========
%% Templates are strings containing $ variables.
%% Here's a small shell dialogue
%% to see how templates can be used:

%% 1> Str = "${who} is feeling ${how}".                                    
%% "${who} is feeling ${how}"
%% 2> Template = elib2_misc:string2template(Str).
%% ["who",<<" is feeling ">>,"how"]
%% 3> elib2_misc:expand_template(Template,[{"who","Sue"},{"how","happy"}]).
%% ["Sue",<<" is feeling ">>,"happy"]

%% Templates are particularly useful when they are stored in files
%% and can be used as the basis for generating web pages and \LaTeX files.
%%
%% expand_file_template(TemplateFile, Assoc, FileOut)
%% Expands TemplateFile with the data in Assoc
%% producing FileOut


%%----------------------------------------------------------------------
%% @doc Expand a string template file, with property list to produce a string.

-spec expand_string_template(Str::string(),
			     PropertyList::[{Key::string(),Val::string()}]) ->
				    string().

expand_string_template(Str, Assoc) ->
    Template = string2template(Str),
    expand_template(Template, Assoc).

%%----------------------------------------------------------------------
%% @doc Expand a template file, with property list to produce a file.

-spec expand_file_template(FileIn::string(),
			   PropertyList::[{Key::string(),Val::string()}],
			   FileOut::string()) ->
    ok.

expand_file_template(FileIn, Assoc, FileOut) ->
    Template = file2template(FileIn),
    template2file(Template, Assoc, FileOut).

%%----------------------------------------------------------------------
%% @doc Expand a template and a property list.

-spec expand_template([binary()|string()],
		      [{string(),string()}]) ->
    [binary()|string()].

expand_template(Template, Assoc) ->
    map(fun(B) when is_binary(B) -> B;
	   (S) when is_list(S) -> get_val(S, Assoc);
	   (S) when is_atom(S) -> get_val(atom_to_list(S), Assoc)
	end, Template).
 
%%----------------------------------------------------------------------
%% @doc
%% Parse a file containing a template.

-spec file2template(FileName::string()) ->
    [binary() | string()].

file2template(File) -> string2template(file2string(File)).

%%----------------------------------------------------------------------
%% @doc
%% Parse a string containing a template.

-spec string2template(FileName::string()) ->
    [binary() | string()].

string2template(Str) -> string2template(Str, []).

string2template_test() ->
    Str = "${who} is feeling ${how}",
    Template = string2template(Str),
    ["who",<<" is feeling ">>,"how"] = Template,
    ["Sue",<<" is feeling ">>,"happy"] = 
	expand_template(Template,[{"who","Sue"},{"how","happy"}]).

string2template([], L) ->
    reverse(L);
string2template([$$,${|T], L) ->
    {Var, T1} = collect_template_var(T, []),
    string2template(T1, [Var|L]);
string2template(T, L) ->
    {Str, T1} = collect_template_str(T, []),
    string2template(T1, template_add(Str, L)).

collect_template_var([$}|T], L) -> {reverse(L), T};
collect_template_var([H|T], L)  -> collect_template_var(T, [H|L]).

collect_template_str([$$,${|_]=X, L) -> {reverse(L), X};
collect_template_str([H|T], L)       -> collect_template_str(T, [H|L]);
collect_template_str([], L)          -> {reverse(L), []}.

template_add("", L) -> L;
template_add(Str, L) -> [list_to_binary(Str)|L].

%%----------------------------------------------------------------------
%% @doc
%% Expand a string containing environment variables
%% If the environment vraoble is not defined an exception is raised

-spec expand_env_vars(string()) -> string().

expand_env_vars(Str) ->
    expand_env_vars(Str, []).

expand_env_vars([], L) ->
    reverse(L);
expand_env_vars([$$,${|T], L) ->
    {Var, T1} = collect_template_var(T, []),
    case os:getenv(Var) of
	false -> exit({noEnvironmentVariable,Var});
	Val   -> expand_env_vars(T1, reverse(Val, L))
    end;
expand_env_vars([H|T], L) ->
    expand_env_vars(T, [H|L]).


%%----------------------------------------------------------------------
%% @doc
%% TeX to PDF
%% Run pdflatex on the created /verb+.tex+ file.
%% In the rare event that you get a syntax error in your generated
%% .tex file then the best thig to do is manually run pdflatex
%% to determine the fault.
%% Runs with output in the directory where File is

-spec tex2pdf(FileName::string()) -> ok.
    
tex2pdf(File) ->
    %% io:format("tex2pdf:~p cwd:~p~n",[File,file:get_cwd()]),
    Pdf = outfile(File, ".pdf"),
    Old = filelib:last_modified(Pdf),
    %% Cmd = "pdflatex -output-directory ../tmp " ++ File,
    OutDir = filename:dirname(File),
    Cmd = "pdflatex >/dev/null -output-directory=" ++ OutDir ++ " " ++ File,
    os:cmd(Cmd),
    os:cmd(Cmd),
    %% io:format("Cmd=~p V=~p~n",[Cmd,V]),
    os:cmd(Cmd),
    New = filelib:last_modified(Pdf),
    %%  check that we have created an updated PDF file
    case New > Old of
	true ->
	    ok;
	false ->
	    Log = outfile(File, ".log"),
	    io:format("Something was wrong: Look in ~s~n",[Log])
    end.

%%----------------------------------------------------------------------
%% @doc
%% file2stream(File) -> Stream raises exit(eNoStream).
%% things to streams
%%   Stream() -> {Str, Stream'} | eos

%% Dialyzer bug ???
%% This is the *real* definition
%% -type stream() :: fun(() -> {string(), stream()} | eos).
%% Kostis suggest this:

-type stream() :: fun( () -> {string(), fun( () -> _)} | eos ).

-spec file2stream(FileName::string()) -> stream().

file2stream(File) ->
    %% read the file 4096 bytes at a time
    %% so we can read infinite files 
    case file:open(File, [binary,raw,read]) of
	{ok, P} -> fun() -> file2stream_1(P) end;
	_   -> exit({eNoFile, File})
    end.

file2stream_1(P) ->
    case file:read(P, 4096) of
	{ok, Bin} ->
	    {binary_to_list(Bin), 
	     fun() -> file2stream_1(P) end};
	eof ->
	    file:close(P),
	    eos
    end.

%%----------------------------------------------------------------------
%% @doc Convert a string to a stream.

-spec string2stream(string()) -> stream().
			   
string2stream(Str) ->
    fun() -> {Str, fun() -> eos end} end.

string2stream_test() ->
    F = string2stream("abc"),
    {"abc", F1} = F(),
    eos = F1().

%%----------------------------------------------------------------------
%% @doc skip all characters up to and including new line

-spec skip_to_nl(string()) -> string().
    
skip_to_nl([$\n|T]) -> T;
skip_to_nl([])      -> [];
skip_to_nl([_|T])   -> skip_to_nl(T).

skip_to_nl_test() ->
    "abc" = skip_to_nl("123 \nabc").

%%----------------------------------------------------------------------
%% @doc extract N lines of Lorum ipsum

-spec lorem(N::integer()) -> [string()].
    
lorem(N) -> lorem(N, [], []).

lorem(0, _, L)     -> reverse(L);
lorem(N, [H|T], L) -> lorem(N-1, T, [H,$\s|L]);
lorem(N, [], L)    -> lorem(N, lorem_data(), L).

lorem_data() -> 
    ["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
     "Aenean id risus ante.", 
     "Integer hendrerit porta justo sed luctus.",
     "Nam vitae ligula enim, non accumsan odio.",
     "Duis viverra pharetra augue, eget tincidunt leo ornare et.",
     "Curabitur a diam nibh, id consequat ante.",
     "Aenean facilisis, lorem a vulputate pulvinar, lorem eros fermentum lacus, nec tincidunt diam purus id metus.",
     "Vivamus sagittis consectetur nisi, eget tincidunt orci ultrices iaculis.",
     "Quisque turpis tellus, feugiat ut tristique laoreet, convallis a orci.",
     "Mauris nec urna vel arcu scelerisque aliquam a id erat.",
     "Nunc eu justo bibendum arcu rhoncus venenatis vitae ut sapien.",
     "Etiam dictum convallis libero, eget dictum leo egestas id.",
     "Aliquam metus orci, bibendum sed lobortis et, tempor at elit.",
     "Aliquam erat volutpat.",
     "Aliquam iaculis tincidunt massa, vel molestie tortor elementum tincidunt.",
     "Quisque ornare consectetur pretium.",
     "Nam aliquet, erat non commodo accumsan, eros urna congue nisl, ac tincidunt elit mi et lacus.",
     "Donec vestibulum metus et nibh lobortis vehicula.",
     "Nunc eros nibh, commodo nec convallis ac, ultricies nec sem.",
     "Cras nisl lectus, venenatis quis ornare ut, venenatis vitae libero.",
     "Nam ante lectus, iaculis quis venenatis at, hendrerit non leo."].


%% These ahve the same names and functionality as the
%% javascript equivalents

encodeURI([H|T]) when ?IN(H,$a,$z) -> [H|encodeURI(T)];
encodeURI([H|T]) when ?IN(H,$A,$Z) -> [H|encodeURI(T)];
encodeURI([H|T]) when ?IN(H,$0,$9) -> [H|encodeURI(T)];
encodeURI([H|T]) when H == $_; H == $.; H == $-; H == $/; H == $: ->
    [H|encodeURI(T)];
encodeURI([H|T]) ->
    [B1,B2] = unsigned_byte_to_hex_string(H),
    [$%,B1,B2|encodeURI(T)];
encodeURI([]) -> [].

decodeURI([$%,Hi,Lo|T]) -> 
    [hex_nibble2int(Hi) * 16 + hex_nibble2int(Lo)|decodeURI(T)];
decodeURI([$+|T])       -> [$ |decodeURI(T)];
decodeURI([H|T])        -> [H|decodeURI(T)];
decodeURI([])           -> [].

%% must are file operations that must suceed otherwise we throw an error

must_ensure_dir(Dir) ->
    case filelib:ensure_dir(Dir ++"/") of
	ok ->
	    %% io:format("Made Dir::~s~n",[Dir]),
	    ok;
	Error ->
	    io:format("*** Error creating directory:~p ~p~n",[Dir, Error]),
	    throw({eCreateDirError, Dir, Error})
    end.
		
must_write_file(File, Bin) ->
    case file:write_file(File, Bin) of
	ok ->
	    %% io:format("Write File::~s~n",[File]),
	    ok;
	Error ->
	    io:format("*** Error writing:~p ~p~n",[File, Error]),
	    throw({eFileWriteError, File, Error})
    end.

must_read_file(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    Bin;
	Error ->
	    io:format("*** Error reading:~p~n",[File]),
	    throw({eFileReadError, File, Error})
    end.

%%----------------------------------------------------------------------    
%% Example:
%%  make_module("foo", "note", [{a,b},{c,d}]) 
%% creates foo.erl
%%         note(a) -> b;
%%         ...

make_module(Mod, FuncName, L) ->
    make_module(Mod, FuncName, L, "").

make_module(Mod, FuncName, L, Extra) ->
    S = ["-module(",Mod,").\n",
	 "-compile(export_all).\n",
	 Extra,
	 func(FuncName, L),
	 FuncName,"(X)->{error,X}.\n"],
    file:write_file(Mod ++ ".erl", S).

func(Name, L) -> 
    [clause(Name, I) || I <- L].

clause(Name, {Key,Val}) ->
    io_lib:format("~s(~p) ->~n     ~p;~n",[Name,Key,Val]).

xml_to_iol({Tag,Args,[]}) ->
    [empty_node(Tag, Args)];
xml_to_iol({Tag,Args,Children}) ->
    [start_node(Tag,Args),
     [xml_to_iol(I) || I <- Children],
     end_node(Tag)];
xml_to_iol(L) when is_list(L) ->
    [xml_to_iol(I) || I <- L];
xml_to_iol({cdata,Str}) ->
    ["<![CDATA[",
     unicode:characters_to_binary(Str, unicode, utf8),
     "]]>"];
xml_to_iol({comment,Str}) ->
    ["<!--",
     unicode:characters_to_binary(Str, unicode, utf8),
     "-->"];
xml_to_iol({processingInstruction,Tag,Str}) ->
    ["<?",Tag," ",
     unicode:characters_to_binary(Str, unicode, utf8),
     "?>"];
xml_to_iol({str,S}) ->
    unicode:characters_to_binary(escape(S), unicode, utf8);
xml_to_iol({ws, S}) ->
    unicode:characters_to_binary(S, unicode, utf8).

%% cdata_node({cdata,S}) ->
%%     unicode:characters_to_binary(escape(S), unicode, utf8).

%% str_node({str,S}) ->
%%     unicode:characters_to_binary(escape(S), unicode, utf8).

%% ws_node({ws,S}) ->
%%     unicode:characters_to_binary(S, unicode, utf8).

start_node(Tag, Args) ->
    ["<", to_str(Tag), [format_args(I) || I <- Args],">"].

empty_node(Tag, Args) ->
    ["<", to_str(Tag), [format_args(I) || I <- Args],"/>"].

format_args({Key,Val}) -> [" ",to_str(Key),"=\"",to_str(Val),"\""].

end_node(Tag) ->     
%% eraad    ["</",Tag,">"].
    ["</",to_str(Tag),">\n"].


escape(B) when is_binary(B) -> escape(binary_to_list(B));
escape([$<|T])              -> "&lt;" ++ escape(T);
escape([H|T])               ->  [H|escape(T)];
escape([])                  ->  [].

to_str(A) when is_atom(A)    -> atom_to_list(A);
to_str(N) when is_integer(N) -> integer_to_list(N);
to_str(X)                    -> X.

%%----------------------------------------------------------------------

glob_dir(Dest) ->
    case filelib:is_dir(Dest) of
	true ->
	    Dest;
	false ->
	    io:format("The directory ~s does not exist~n"
		      "Please create it you wish to run this program~n",
		      [Dest]),
	    exit({eNoDir,Dest})
    end.

