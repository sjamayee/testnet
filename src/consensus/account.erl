-module(account).
-export([serialize/1,deserialize/1,new/4,nonce/1,write/2,get/2,update/5,addr/1,id/1,balance/1,root_hash/1,now_balance/3,delete/2,test/0]).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      height = 0,  %The last height at which you paid the tax
	      addr = [], %addr is the hash of the public key we use to spend money.
	      id = 0}). %id is your location in the merkle trie. It is also used as an identification for sending money, since it is shorter than your address.
-spec addr(#acc{}) -> any().
addr(X) -> X#acc.addr.
-spec id(#acc{}) -> any().
id(X) -> X#acc.id.
-spec balance(#acc{}) -> any().
balance(X) -> X#acc.balance.
-spec now_balance(#acc{balance::number(),height::number()},number(),number()) -> number().
now_balance(Acc, Amount, NewHeight) ->
    OldHeight = Acc#acc.height,
    Rent = constants:account_rent()*(NewHeight - OldHeight),
    Amount + Acc#acc.balance - Rent.
    
-spec update(number(),_,number(),_,number()) -> #acc{balance::number(),height::number(),addr::binary(),id::char()}.
update(Id, Accounts, Amount, NewNonce, NewHeight) ->
    {_, Acc, _} = get(Id, Accounts),
    OldNonce = Acc#acc.nonce,
    FinalNonce = case NewNonce of
	none -> Acc#acc.nonce;
	N -> true = N > OldNonce,
	     N
    end,
    OldHeight = Acc#acc.height,
    true = NewHeight >= OldHeight,
    NewBalance = now_balance(Acc, Amount, NewHeight),
    true = NewBalance > 0,
    Acc#acc{balance = NewBalance,
	 nonce = FinalNonce,
	 height = NewHeight}.
-spec new(_,_,_,_) -> #acc{nonce::0}.
new(Id, Addr, Balance, Height) ->
    #acc{id = Id, addr = Addr, balance = Balance, nonce = 0, height = Height}.
-spec nonce(#acc{}) -> any().
nonce(X) -> X#acc.nonce.
-spec serialize(#acc{balance::integer(),nonce::integer(),height::integer(),addr::binary(),id::integer()}) -> <<_:64,_:_*8>>.
serialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    Addr = A#acc.addr,
    Baddr = testnet_sign:address2binary(Addr),
    SizeAddr = size(Baddr),
    SizeAddr = constants:hash_size(),
    Nbits = constants:account_nonce_bits(),
    AP = constants:account_padding(),
    KL = key_length(),
    ID = A#acc.id,
    true = (ID - 1) < math:pow(2, KL),
    Out = <<(A#acc.balance):BAL, 
	    (A#acc.nonce):(Nbits), 
	    (A#acc.height):HEI,
	    ID:KL,
	    Baddr/binary,
	    0:AP>>,
    Size = size(Out),
    Size = constants:account_size(),
    Out.

-spec deserialize(<<_:208>>) -> #acc{balance::non_neg_integer(),nonce::char(),height::non_neg_integer(),addr::binary(),id::char()}.
deserialize(A) ->
    BAL = constants:balance_bits(),
    HEI = constants:height_bits(),
    HD = constants:hash_size()*8,
    Nbits = constants:account_nonce_bits(),
    AP = constants:account_padding(),
    KL = constants:key_length(),
    <<B1:BAL,
      B2:Nbits,
      B4:HEI,
      B5:KL,
      B6:HD,
      _:AP>> = A,
    #acc{balance = B1, nonce = B2, height = B4, id = B5, addr = testnet_sign:binary2address(<<B6:HD>>)}.
    
-spec write(_,#acc{balance::integer(),nonce::integer(),height::integer(),addr::binary(),id::integer()}) -> any().
write(Root, Account) ->%These are backwards.
    ID = Account#acc.id,
    M = serialize(Account),
    trie:put(ID, M, 0, Root, accounts).%returns a pointer to the new root.
-spec delete(_,_) -> any().
delete(ID, Accounts) ->
    trie:delete(ID, Accounts, accounts).
-spec key_length() -> 11.
key_length() ->
    constants:key_length().
-spec get(number(),_) -> {_,'empty' | #acc{balance::non_neg_integer(),nonce::char(),height::non_neg_integer(),addr::binary(),id::char()},_}.
get(Id, Accounts) ->
    %true = ID > 0,
    true = (Id - 1) < math:pow(16, key_length()),
    {RH, Leaf, Proof} = trie:get(Id, Accounts, accounts),
    V = case Leaf of
	    empty -> empty;
	    L -> deserialize(leaf:value(L))
	end,
    {RH, V, Proof}.

-spec root_hash(_) -> any().
root_hash(Accounts) ->
    trie:root_hash(accounts, Accounts).

-spec test() -> 'success'.
test() ->
    {Address, _Pub, _Priv} = testnet_sign:hard_new_key(),
    ID = 3,
    Acc = new(ID, Address, 0, 0),
    %io:fwrite(Acc),
    S = serialize(Acc),
    Acc = deserialize(S),
    NewLoc = write(0, Acc),
    {_, Acc, _} = get(ID, NewLoc),
    success.
