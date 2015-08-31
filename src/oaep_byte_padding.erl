-module(oaep_byte_padding).

-compile(export_all).

%% Optimal asymmetric encryption padding
%% We assume N = 100 (an 800 bit modulus)

%% smarter and faster padding
%% The message is always an exact number of bytes long
%% (ie not Bits)
%% salt is not used
%% K0 = 20

%% Assume 800 bit key 

%%  Modulus N     = 100 bytes
%%  K0            = 20 bytes
%%  K1            = padding (at least 1 byte)
%%  Max payload   = 100 - 21 bytes (= 79 bytes) 

%%  A 64 byte message would be like this

%%  |<---------------------------N = 100 bytes -------------------------->|
%%  
%%  +----------------------+   +------------------+   +-------------------+
%%  |   Message (79 bytes) |   |   Pad (K1) bytes |   |   (K0 = 20) bytes |
%%  |          M           |   |   00 00 00 04    |   |        R          |
%%  +----------------------+   +------------------+   +-------------------+
%%             | N-K1-K0 bytes         | K1 bytes              |
%%             |                       |                       |
%%             +-----------+-----------+                      \|/
%%                         |                                   |
%%                        XOR ------<------- G ------<---------+
%%                         |                                   |
%%                         |------------>--- H ------>--------XOR
%%                         |                                   |
%%                         X  (N-K0) bytes                     Y K0 bytes
%%                         


%% this is for a fixed 800 bit (100 byte modulus)


test() ->
    timer:tc(?MODULE, test1, [<<"hello world">>]).


test1(Msg) ->
    B = pad(Msg, 100, 20),
    io:format("size(B) = ~p~n",[size(B)]),
    Msg = unpad(B, 20),
    yes.

%% pad(Msg, N, K0) 
%% Msg is binary
%% N is the size of the modulus in bytes
%% K0 is the size in bytes of a random string that is generated for
%%    each evocation of the algorithm 

pad(Msg, N, K0) when is_binary(Msg), is_integer(N), 
		     is_integer(K0) ->
    NPad = N - K0 - size(Msg),
    Pad  = make_padding(NPad),
    R    = crypto:strong_rand_bytes(K0),
    G    = g(N-K0, R),
    M0   = <<Msg/binary, Pad/binary>>,
    X    = xor1(G, M0),
    H    = h(X, K0),
    Y    = xor1(R, H),
    <<X/binary, Y/binary>>.

unpad(B, K0) ->
    Len = size(B) - K0,
    {X, Y} = split_binary(B, Len),
    R      = xor1(h(X, K0), Y),
    Mpad   = xor1(X, g(Len, R)),
    unpad1(Mpad).

unpad1(Bin) ->
    Len = size(Bin),
    {_,<<N>>} = split_binary(Bin, Len - 1),
    {B1, _}   = split_binary(Bin, Len - N),
    B1.

%% h(B, K) Hash B and reduce the size to K bytes
%%   Note K is chosen to be 20 bytes
%%   so we just use sha to reduce the hash

h(B, 20) ->
    crypto:hash(sha, B).

xor1(B1, B2) ->
    L1 = binary_to_list(B1),
    L2 = binary_to_list(B2),
    L = lists:zip(L1, L2),
    list_to_binary([A bxor B || {A,B} <- L]).

g(N, R) ->
    X0 =  crypto:hash(sha, R),
    g1(X0, N, 1).

g1(X, N, _) when size(X) > N ->
    <<B:N/binary,_/binary>> = X,
    B;
g1(X, N, K) ->
    Step = crypto:hash(sha, <<X/binary,K:32>>),
    X1 = <<X/binary,Step/binary>>,
    g1(X1, N, K+1).


make_padding(N) -> list_to_binary(make_padding(N-1, [N])).

make_padding(0, L) -> L; 
make_padding(N, L) -> make_padding(N-1, [0|L]).
