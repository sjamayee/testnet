-module(active_oracles).
-export([new/2,write/2,get/2,id/1,question/1,starts/1,
	 root_hash/1, test/0]).


%These are the oracles that allow betting right now.
%This data is available to the VM.

-record(active, {id = 0, %this is the id that the active oracle is stored under. oracles never reuse the same id.
		 question, %this is the hash of the text of the question that was asked of the oracle. The full question was written on the block that started the oracle.
		 starts}). %this is the block height when trading begins.

id(X) -> X#active.id.
question(X) -> X#active.question.
starts(X) -> X#active.starts.

new(Question, Starts) ->
    ID = make_id(),
    #active{id = ID, question = Question, starts = Starts}.
make_id()->
    0.

serialize(A) ->
    HEI = constants:height_bits(),
    Pad = constants:active_oracles_padding(),
    HS = constants:hash_size(),
    KL = constants:key_length(),
    <<(A#active.id):KL,
      (A#active.question):HS,
      (A#active.starts):HEI, 
      0:Pad>>.
deserialize(B) ->
    HEI = constants:height_bits(),
    Pad = constants:active_oracles_padding(),
    HS = constants:hash_size(),
    KL = constants:key_length(),
    <<ID:KL, Q:HS, A:HEI, 0:Pad>> = B,
    #active{id = ID, question = Q, starts = A}.

write(Oracle, Root) ->
    V = serialize(Oracle),
    Key = Oracle#active.id,
    trie:put(Key, V, 0, Root, active_oracles).
get(ID, Root) ->
    true = ID > 0,
    true = ID < math:pow(2, constants:key_length()),
    {RH, Leaf, Proof} = trie:get(ID, Root, active_oracles),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.

root_hash(Root) ->
    trie:root_hash(active_oracle, Root).
	       
    
test() ->
    X = new(1, 2),
    X = deserialize(serialize(X)),
    NewLoc = write(0, X),
    {_, X, _} = get(X#active.id, NewLoc),
    success.
    
