-module(oracles).
-export([new/5,write/3,get/2,id/1,result/1,
	 question/1,starts/1,
	 root_hash/1, test/0]).
-define(name, oracles).
-record(oracle, {id, result, question, starts, orders}).
%we need to store a pointer to the orders tree in the meta data.

id(X) -> X#oracle.id.
result(X) -> X#oracle.result.
question(X) -> X#oracle.question.
starts(X) -> X#oracle.starts.
new(ID, Result, Question, Starts, OrdersTree) ->
    %Orders = orders:root_hash(OrdersTree),
    Orders = OrdersTree,
    #oracle{id = ID,
	    result = Result,
	    question = Question,
	    starts = Starts,
	    orders = Orders
	   }.
root_hash(Root) ->
    trie:root_hash(?name, Root).
serialize(X) ->
    KL = constants:key_length(),
    HS = constants:hash_size(),
    Question = X#oracle.question,
    Orders = X#oracle.orders,
    HS = size(Question),
    HS = size(Orders),
    HEI = constants:height_bits(),
    <<(X#oracle.id):KL,
      (X#oracle.result):8,
      (X#oracle.starts):HEI,
      Question/binary,
      Orders/binary>>.
deserialize(X) ->
    KL = constants:key_length(),
    HS = constants:hash_size()*8,
    HEI = constants:height_bits(),
    <<ID:KL,
      Result:8,
      Starts:HEI,
      Question:HS,
      Orders:HS
    >> = X,
    #oracle{
       id = ID,
       result = Result,
       starts = Starts,
       question = <<Question:HS>>,
       orders = <<Orders:HS>>
      }.
write(Oracle, Root, Meta) ->
    %meta is a pointer to the orders tree.
    V = serialize(Oracle),
    Key = Oracle#oracle.id,
    trie:put(Key, V, Meta, Root, ?name).
get(ID, Root) ->
    {RH, Leaf, Proof} = trie:get(ID, Root, ?name),
    {V, Meta} = case Leaf of 
	    empty -> {empty, 0};
	    L -> {deserialize(leaf:value(L)),
		  leaf:meta(L)}
	end,
    {RH, V, Proof, Meta}.


test() ->
    Root = 0,
    X = new(1,2, testnet_hasher:doit(1), 2, testnet_hasher:doit(2)),
    X = deserialize(serialize(X)),
    Meta = 0,
    NewLoc = write(X, Root, Meta),
    {_, X, _, Meta} = get(X#oracle.id, NewLoc),
    {_, empty, _, 0} = get(X#oracle.id, 0),
    success.
    
