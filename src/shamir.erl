%% From: https://github.com/rnewson/shamir/tree/master/src/shamir.erl

%% Copyright 2011 Robert Newson
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(shamir).

-export([share/3, recover/1]).
-include("shamir.hrl").

share(Secret, Threshold, Count) when is_binary(Secret) ->
    share(binary_to_list(Secret), Threshold, Count);

share(Secret, Threshold, Count) when is_list(Secret) ->
    Shares = transpose([share(Byte, Threshold, Count) || Byte <- Secret]),
    [#share{threshold=Threshold, x=X, y=list_to_binary(lists:nth(X, Shares))}
        || X <- lists:seq(1, Count)];

share(Secret, Threshold, Count) when (Secret >= 0 andalso Secret =< 255) ->
    GF = galois:generate(8),
    Coeffs = binary_to_list(crypto:rand_bytes(Threshold - 1)) ++ [Secret],
    [horner(GF, X, Coeffs) || X <- lists:seq(1, Count)].

horner(GF, X, Coeffs) ->
    horner(GF, X, Coeffs, 0).

horner(_, _, [], Acc) ->
    Acc;
horner(GF, X, [Coeff|Rest], Acc) ->
    Mult = galois:multiply(GF, Acc, X),
    Add = galois:add(GF, Mult, Coeff),
    horner(GF, X, Rest, Add).

recover([#share{threshold=Threshold}|_]=Shares0) ->
    Shares = lists:ukeysort(#share.x, Shares0),
    X = [X || #share{x=X} <- Shares],
    Ys = transpose(lists:map(fun(#share{y=Y}) ->
        binary_to_list(Y) end, Shares)),
    list_to_binary([recover(Threshold, Z) || Z <- [lists:zip(X, Y) || Y <- Ys]]).

recover(Threshold, Shares) when length(Shares) >= Threshold ->
    lagrange(lists:sublist(Shares, Threshold)).

lagrange(Shares) ->    
    GF = galois:generate(8),
    lists:foldl(fun(Share, Acc) ->
        galois:add(GF, lagrange(GF, Share, Shares), Acc) end,
        0, Shares).

lagrange(GF, Share, Shares) ->
    lagrange(GF, Share, Shares, 1).

lagrange(GF, {_, Y}, [], Acc) ->
    galois:multiply(GF, Y, Acc);
lagrange(GF, {X, Y}, [{X, _} | Rest], Acc) ->
    lagrange(GF, {X, Y}, Rest, Acc);
lagrange(GF, {X1, Y1}, [{X2, _} | Rest], Acc) ->
    lagrange(GF, {X1, Y1}, Rest,
        galois:multiply(GF, Acc,
            galois:divide(GF, X2,
                galois:subtract(GF, X1, X2)))).

transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]]
     | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [].
