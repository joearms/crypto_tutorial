-module(plot_random).
-compile(export_all).

%% +-----------------+
%% |                 |
%% |  +-----------+  |
%% |  |           |  |
%% |  |           |  |
%% |  +-----------+  |
%% |                 |
%% |  Text           |
%% +-----------------+


test1() ->
    Margin = 30,
    TxtHt = 15,
    Width = 256 + 2*Margin,
    Ht = 256 + 2*Margin + TxtHt,
    Img = egd:create(Width,Ht),
    Red = egd:color({255,0,0}),
    Blue = egd:color({0,0,255}),
    egd:rectangle(Img,{Margin,Margin},{Margin+256,Margin+256},Red),
    Filename = filename:join([code:priv_dir(percept),"fonts","6x11_latin1.wingsfont"]),
    Font = egd_font:load(Filename),
    egd:text(Img, {Margin,Margin+256}, Font,"This is a plot",Blue),
    file:write_file("foo.png", [egd:render(Img, png)]),
    egd:destroy(Img).

test2() ->
    %% with an iterator
    plot("lgc1.png", 256*256 div 5, fun lcg1/2, 
	 "Linear Congruent Generator", "silly"),
    plot("crypto1.png", 256*256 div 5, fun crypto1/2 , 
	 "Erlang crypto:rand_bytes()","silly"),
    %% takes a huge time  (26 mins)
    io:format("wait 26 minutes~n"),
    plot("timer1.png", 256*256 div 5, fun timer1/2, 
	 "Erlang os:timestamp() and timer:sleep()", "silly"),
    ok.

plot(File, K, F, Title, Password) ->
    Margin = 30,
    TxtHt = 15,
    Width = 256 + 2*Margin,
    Ht = 256 + 2*Margin + TxtHt,
    Img = egd:create(Width,Ht),
    Red = egd:color({255,0,0}),
    Blue = egd:color({0,0,255}),
    Black = egd:color({0,0,0}),
    S0 = F(pwd, Password),
    egd:rectangle(Img,{Margin,Margin},{Margin+256,Margin+256},Red),
    Plot = fun(B1, B2) ->
		   XX = B1 + Margin,
		   YY = B2 + Margin,
		   egd:rectangle(Img,{XX,YY},{XX,YY},Black)
	   end,
  
    Filename = filename:join([code:priv_dir(percept),"fonts","6x11_latin1.wingsfont"]),
    Font = egd_font:load(Filename),
    egd:text(Img, {Margin,Margin+256}, Font,Title,Black),
    add_points(K, Plot, Black, F, S0),
    file:write_file(File, [egd:render(Img, png)]),
    io:format("written:~s~n",[File]),
    egd:destroy(Img).
    
add_points(0,_,_,_,_) ->
    true;
add_points(K, Plot, Black, F, S0) ->
    {B1,S1} = F(next, S0),
    {B2,S2} = F(next, S1),
    Plot(B1,B2),
    add_points(K-1, Plot, Black, F, S2).

lcg1(pwd, Password) ->
    password_to_int(Password);
lcg1(next, X) ->
    X1 = lcg(X),
    B = next_byte(X1),
    {B, X1}.
    
timer1(pwd,_) -> 1;
timer1(next, S) -> 
    {rndbyte(), S}.

%% randbyte() due to Richard Carlsson [private communication]

%% START:rndbyte
rndbyte() ->
    <<N:8>> = << <<B:1/integer>> || B <- rndbits(8) >>,
    N.

rndbits(N) -> [rndbit() || _<-lists:seq(1,N)].

rndbit() ->
    timer:sleep(1),
    element(3, os:timestamp()) rem 2.
%% END:rndbyte

crypto1(pwd, _) -> 1;
crypto1(next, S) -> 
    <<B:8>> = crypto:rand_bytes(1),
    {B, S}.

password_to_int(Str) ->
    erlang:phash(Str, 4294967296).

lcg(X) ->
    A = 1664525,
    C = 1013904223,
    M = 4294967296,   %% 2^32
    (X*A + C) rem M.

next_byte(N) ->
    (N bsr 4) band 255.

