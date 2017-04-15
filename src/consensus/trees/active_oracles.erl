-module(active_oracles).
-export([new/3,write/2,get/2,id/1,question/1,starts/1,
	 root_hash/1, test/0]).


%These are the oracles that allow betting right now.
%This data is available to the VM.

-record(active, {id = 0, %this is the id that the active oracle is stored under. oracles never reuse the same id.
		 question, %this is the hash of the text of the question that was asked of the oracle. The full question was written on the block that started the oracle.
		 starts}). %this is the block height when trading begins.

id(X) -> X#active.id.
question(X) -> X#active.question.
starts(X) -> X#active.starts.

new(Question, Starts, Root) ->
    ID = make_id(Root),
    #active{id = ID, question = Question, starts = Starts}.
make_id(Root)->
    KL = constants:key_length(),
    Max = round(math:pow(2, KL)),
    R = crypto:rand_uniform(1, Max),
    {_, E, _} = get(R, Root),
    case E of
	empty -> R;
	true -> make_id(Root)
    end.

serialize(A) ->
    HEI = constants:height_bits(),
    Pad = constants:active_oracles_padding(),
    HS = constants:hash_size(),
    Question = A#active.question,
    HS = size(Question),
    KL = constants:key_length(),
    <<(A#active.id):KL,
      (A#active.starts):HEI,
      0:Pad,
      Question/binary>>.
deserialize(B) ->
    HEI = constants:height_bits(),
    Pad = constants:active_oracles_padding(),
    HS = constants:hash_size()*8,
    KL = constants:key_length(),
    <<ID:KL, A:HEI, _:Pad, Q:HS>> = B,
    #active{id = ID, question = <<Q:HS>>, starts = A}.

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
    Root = 0,
    X = new(testnet_hasher:doit(1), 2, Root),
    X = deserialize(serialize(X)),
    NewLoc = write(X, Root),
    {_, X, _} = get(X#active.id, NewLoc),
    success.
    
