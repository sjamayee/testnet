-module(existence).
-export([get/2,write/3, test/0]).
%for accessing the proof of existence tree
-record(exist, {address, hash}).

serialize(E) ->
    AB = constants:acc_bits(),
    HS = constants:hash_size(),
    Pad = constants:existence_pad(),
    Address = E#exist.address,
    Hash = E#exist.hash,
    AB = size(Address),
    HS = size(Hash),
    <<Address/binary,
      Hash/binary,
      0:Pad>>.
deserialize(B) ->
    AB = constants:acc_bits(),
    HS = constants:hash_size(),
    Pad = constants:existence_pad(),
    <<Add:AB, Hash:HS, _:Pad>> = B,
    #exist{address = <<Add:AB>>,
	   hash = <<Hash:HS>>}.

get(Hash, Tree) ->
    true = is_binary(Hash),
    Key = hash2int(Hash),
    {X, Leaf, Proof} = trie:get(Key, Tree, existence),
    V = case Leaf of
	    empty -> empty;
	    L -> 
		Y = leaf:value(L),
		deserialize(Y)
	end,
    {X, V, Proof}.
write(N, E, Tree) ->
    Hash = E#exist.hash,
    Key = hash2int(Hash),
    X = serialize(E),
    trie:put(Key, X, 0, Tree, existence).
	     
hash2int(X) -> 
    S = size(X),
    S = constants:hash_size(),
    hash2int(X, 0).
hash2int(<<>>, N) -> N;
hash2int(<<X, Y/binary>>, N) ->
    M = (N*256) + X,
    hash2int(Y, M).


test() ->
    C = testnet_hasher:doit(2),
    {_, empty, _} = get(C, 0),
    Key = 1,
    NewLoc = write(Key, C, 0),
    NewLoc2 = write(Key, testnet_hasher:doit(4), NewLoc),
    {_, Key, _} = get(C, NewLoc2),
    {_, empty, _} = get(C, 0),
    success.
