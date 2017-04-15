-module(oracle_results).
-export([new/3,write/2,get/2,id/1,result/1,root_hash/1,
	 test/0]).
%These are the results of oracles that have existed. They are stored by id.
%This data is available to the VM.
%The result is stored in 1 byte. Either it is 0 for false, 1 for true, or 2 if the questions was bad.
-define(name, oracle_results).
-record(results, {id = 0, result}).

id(X) -> X#results.id.
result(X) -> X#results.result.
new(ID, Result, Root) ->
    {_, E, _} = get(ID, Root),
    empty = E,
    #results{id = ID, result = Result}.
serialize(A) ->
    KL = constants:key_length(),
    <<(A#results.id):KL,
      (A#results.result):8>>.
deserialize(B) ->
    KL = constants:key_length(),
    <<ID:KL, R:8>> = B,
    #results{id = ID, result = R}.
write(A, Root) ->
    V = serialize(A),
    Key = A#results.id,
    trie:put(Key, V, 0, Root, ?name).
get(ID, Root) ->
    true = ID > 0,
    {RH, Leaf, Proof} = trie:get(ID, Root, ?name),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.
root_hash(Root) ->
    trie:root_hash(?name, Root).

test() ->
    Root = 0,
    X = new(1, 0, Root),
    X = deserialize(serialize(X)),
    NewLoc = write(X, Root),
    {_, X, _} = get(X#results.id, NewLoc),
    success.
