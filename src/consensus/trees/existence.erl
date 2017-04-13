-module(existence).
-export([get/2,write/3, test/0]).
%for accessing the proof of existence tree

get(Hash, Tree) ->
    true = is_binary(Hash),
    ID = hash2ID(Hash),
    {X, Leaf, Proof} = trie:get(ID, Tree, existence),
    V = case Leaf of
	    empty -> empty;
	    L -> 
		Y = leaf:value(L),
		AB = constants:acc_bits(),
		<<Z:AB>> = Y,
		Z
	end,
    {X, V, Proof}.
write(N, Hash, Tree) ->
    ID = hash2ID(Hash),
    AB = constants:acc_bits(),
    trie:put(ID, <<N:AB>>, 0, Tree, existence).
	     
hash2ID(X) -> 
    S = size(X),
    S = constants:hash_size(),
    hash2ID(X, 0).
hash2ID(<<>>, N) -> N;
hash2ID(<<X, Y/binary>>, N) ->
    M = (N*256) + X,
    hash2ID(Y, M).


test() ->
    C = testnet_hasher:doit(2),
    {_, empty, _} = get(C, 0),
    ID = 1,
    NewLoc = write(ID, C, 0),
    NewLoc2 = write(ID, testnet_hasher:doit(4), NewLoc),
    {_, ID, _} = get(C, NewLoc2),
    {_, empty, _} = get(C, 0),
    success.
