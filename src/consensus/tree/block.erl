-module(block).
-export([hash/1,check2/1,test/0,mine_test/0,genesis/0,
	 make/3,mine/2,height/1,accounts/1,channels/1,
	 accounts_hash/1,channels_hash/1,
	 read/1,binary_to_file/1,block/1,prev_hash/2,
	 prev_hash/1,read_int/1,check1/1,pow_block/1,
	 mine_blocks/2, hashes/1, 
	 guess_number_of_cpu_cores/0
	]).

-record(block, {height, prev_hash = 0, txs, channels, 
		accounts, mines_block, time, 
		difficulty, comment = <<>>,
		magic = constants:magic()}).%tries: txs, channels, census, 
-record(block_plus, {block, accounts, channels, accumulative_difficulty = 0, prev_hashes = {}}).%The accounts and channels in this structure only matter for the local node. they are pointers to the locations in memory that are the root locations of the account and channel tries on this node.
%prev_hash is the hash of the previous block.
%this gets wrapped in a signature and then wrapped in a pow.
-spec hashes(#block_plus{}) -> any().
hashes(BP) ->
    BP#block_plus.prev_hashes.
    
-spec block(tuple()) -> any().
block(P) when element(1, P) == pow ->
    pow:data(P);
block(BP) when is_record(BP, block_plus) ->
    block(BP#block_plus.block);
block(B) when is_record(B, block) -> B.
-spec pow_block(tuple()) -> tuple().
pow_block(B) when element(1, B) == pow -> B;
pow_block(BP) when is_record(BP, block_plus) ->
    pow_block(BP#block_plus.block).

-spec channels(#block_plus{}) -> any().
channels(Block) ->
    Block#block_plus.channels.
-spec channels_hash(#block_plus{} | #block{}) -> any().
channels_hash(BP) when is_record(BP, block_plus) ->
    channels_hash(pow:data(BP#block_plus.block));
channels_hash(Block) -> Block#block.channels.
-spec accounts(#block_plus{}) -> any().
accounts(BP) ->
    BP#block_plus.accounts.
-spec accounts_hash(#block_plus{} | #block{}) -> any().
accounts_hash(BP) when is_record(BP, block_plus) ->
    accounts_hash(pow:data(BP#block_plus.block));
accounts_hash(Block) ->
    Block#block.accounts.
-spec height(tuple()) -> any().
height(X) ->
    B = block(X),
    B#block.height.
-spec prev_hashes(binary()) -> tuple().
prev_hashes(PH) ->
    H = height(read(PH)),
    prev_hashes([PH], H, 2).
-spec prev_hashes([any(),...],number(),pos_integer()) -> tuple().
prev_hashes([PH|Hashes], Height, N) ->
    NHeight = Height - N,
    if
	NHeight < 1 -> list_to_tuple(lists:reverse([PH|Hashes]));
	true ->
	    B = read_int(NHeight, PH),
	    prev_hashes([hash(B)|[PH|Hashes]], NHeight, N*2)
    end.

   
-spec prev_hash(non_neg_integer(),tuple()) -> any().
prev_hash(0, BP) ->
    prev_hash(BP);
prev_hash(N, BP) ->%N=0 should be the same as prev_hash(BP)
    element(N, BP#block_plus.prev_hashes).
-spec prev_hash(tuple()) -> any().
prev_hash(X) -> 
    B = block(X),
    B#block.prev_hash.
-spec hash(tuple()) -> any().
hash(X) -> 
    testnet_hasher:doit(term_to_binary(block(X))).
-spec time_now() -> integer().
time_now() ->
    (os:system_time() div (1000000 * constants:time_units())) - constants:start_time().
-spec genesis() -> #block_plus{block::{'pow',#block{height::0,prev_hash::0,txs::[],mines_block::1,time::0,difficulty::4080,comment::<<_:752>>,magic::1},4080,44358461744572027408730},channels::0,accumulative_difficulty::0,prev_hashes::{}}.
genesis() ->
    %the pointer to an empty trie is 0.
    Address = constants:master_address(),
    ID = 1,
    First = account:new(ID, Address, constants:initial_coins(), 0),
    Accounts = account:write(0, First),
    AccRoot = account:root_hash(Accounts),
    ChaRoot = channel:root_hash(0),
    Comment = <<"Bitcoin Hits All-Time High as Currency Controls Drive Fear - Bloomberg Online, January 4, 2017">>,

    %Block = 
    %#block{height = 0,
	       %txs = [],
	       %channels = ChaRoot,
	       %accounts = AccRoot,
	       %mines_block = ID,
	       %time = 0,
	       %difficulty = constants:initial_difficulty()},
    Block = {pow,{block,0,0,[], ChaRoot, AccRoot,
		  %<<1,223,2,81,223,207,12,158,239,5,219,253>>,
		  %<<108,171,180,35,202,56,178,151,11,85,188,193>>,
		  1,0,4080, Comment, constants:magic()},
	     4080,44358461744572027408730},
    #block_plus{block = Block, channels = 0, accounts = Accounts}.
    
-spec absorb_txs(#block_plus{},number() | {number(),_},_,[{'signed',tuple(),binary() | [any()],binary() | [any()],_,_,_}]) -> {_,_}.
absorb_txs(PrevPlus, MinesBlock, Height, Txs) ->
    OldAccounts = PrevPlus#block_plus.accounts,
    NewMiner = 
	case MinesBlock of
	    {ID, Address} -> %for miners who don't yet have an account.
		{_, empty, _} = account:get(ID, OldAccounts),
		%We should also give the miner the sum of the transaction fees.
		account:new(ID, Address, constants:block_reward(), Height);
	    MB -> %If you already have an account.
		account:update(MB, OldAccounts, constants:block_reward(), none, Height)
	end,
    NewAccounts = account:write(OldAccounts, NewMiner),
    txs:digest(Txs, 
	       PrevPlus#block_plus.channels,
	       NewAccounts,
	       Height).
    
-spec make(binary(),[{'signed',tuple(),binary() | [any()],binary() | [any()],_,_,_}],number() | {number(),_}) -> #block_plus{block::#block{height::number(),prev_hash::binary(),txs::[{_,_,_,_,_,_,_}],mines_block::number() | {number(),_},time::integer(),comment::<<>>,magic::1},accumulative_difficulty::number(),prev_hashes::tuple()}.
make(PrevHash, Txs, ID) ->%ID is the user who gets rewarded for mining this block.
    ParentPlus = read(PrevHash),
    Parent = pow:data(ParentPlus#block_plus.block),
    Height = Parent#block.height + 1,
    {NewChannels, NewAccounts} = absorb_txs(ParentPlus, ID, Height, Txs),
    CHash = channel:root_hash(NewChannels),
    AHash = account:root_hash(NewAccounts),
    NextDifficulty = next_difficulty(ParentPlus),
    #block_plus{
       block = 
	   #block{height = Height,
		  prev_hash = PrevHash,
		  txs = Txs,
		  channels = CHash,
		  accounts = AHash,
		  mines_block = ID,
		  time = time_now()-5,
		  difficulty = NextDifficulty},
       accumulative_difficulty = next_acc(ParentPlus, NextDifficulty),
       channels = NewChannels, 
       accounts = NewAccounts,
       prev_hashes = prev_hashes(PrevHash)
      }.
-spec next_acc(#block_plus{accumulative_difficulty::number()},_) -> number().
next_acc(Parent, ND) ->
    Parent#block_plus.accumulative_difficulty + pow:sci2int(ND).
    %We need to reward the miner the sum of transaction fees.
-spec mine(#block_plus{block::#block_plus{block::#block_plus{block::{_,_,_,_,_,_} | {_,_,_,_,_,_,_,_,_,_,_}} | #block{}} | #block{}} | #block{},_) -> any().
mine(BP, Times) when is_record(BP, block_plus) ->
    Block = BP#block_plus.block,
    case mine(Block, Times) of
	false -> false;
	Mblock -> BP#block_plus{block = Mblock}
    end;
mine(Block, Times) ->
    Difficulty = Block#block.difficulty,
    pow:pow(Block, Difficulty, Times, constants:hash_size()).

-spec next_difficulty(#block_plus{}) -> any().
next_difficulty(ParentPlus) ->
    Parent = pow:data(ParentPlus#block_plus.block),
    Height = Parent#block.height + 1,
    RF = constants:retarget_frequency(),
    X = Height rem RF,
    OldDiff = Parent#block.difficulty,
    PrevHash = hash(ParentPlus),
    if
	Height == 1 -> constants:initial_difficulty(); 
	Height < (RF+1) -> OldDiff;
	X == 0 -> retarget(PrevHash, Parent#block.difficulty);
	true ->  OldDiff
    end.
-spec median([any()]) -> any().
median(L) ->
    S = length(L),
    F = fun(A, B) -> A > B end,
    Sorted = lists:sort(F, L),
    lists:nth(S div 2, Sorted).
    
-spec retarget(_,_) -> any().
retarget(PrevHash, Difficulty) ->    
    F = constants:retarget_frequency() div 2,
    {Times1, Hash2000} = retarget2(PrevHash, F, []),
    {Times2, _} = retarget2(Hash2000, F, []),
    M1 = median(Times1),
    M2 = median(Times2),
    Tbig = M1 - M2,
    T = Tbig div F,
    %io:fwrite([Ratio, Difficulty]),%10/2, 4096
    ND = pow:recalculate(Difficulty, constants:block_time(), max(1, T)),
    max(ND, constants:initial_difficulty()).
-spec retarget2(_,char(),[any()]) -> {[any()],_}.
retarget2(Hash, 0, L) -> {L, Hash};
retarget2(Hash, N, L) -> 
    BP = read(Hash),
    B = block(BP),
    T = B#block.time,
    H = B#block.prev_hash,
    retarget2(H, N-1, [T|L]).
   
-spec check1(tuple()) -> {_,_}.
check1(BP) -> 
    %check1 makes no assumption about the parent's existance.
    BH = hash(BP),
    GH = hash(genesis()),
    if
	BH == GH -> {BH, 0};
	true ->
	    PowBlock = pow_block(BP),
	    Block = block(PowBlock),
	    %io:fwrite(packer:pack(Block)),
	    Difficulty = Block#block.difficulty,
	    true = Difficulty >= constants:initial_difficulty(),
	    pow:above_min(PowBlock, Difficulty, constants:hash_size()),
 
	    true = Block#block.time < time_now(),
	    true = Block#block.time > 0,%should be replaced with > median of last 100 blocks.
	    {BH, Block#block.prev_hash}
    end.

-spec check2(tuple()) -> #block_plus{block::tuple(),accumulative_difficulty::number(),prev_hashes::tuple()}.
check2(BP) when is_record(BP, block_plus) ->
    check2(pow_block(BP));
check2(PowBlock) ->
    %check that the time is later than the median of the last 100 blocks.

    %check2 assumes that the parent is in the database already.
    %add comment to blocks.
    true = pow:check_pow(PowBlock, constants:hash_size()),
    Block = block(PowBlock),
    true = is_binary(Block#block.comment),
    true = size(Block#block.comment) < constants:comment_limit(),
    true = Block#block.magic == constants:magic(),
    Difficulty = Block#block.difficulty,
    PH = Block#block.prev_hash,
    ParentPlus = read(PH),
    %io:fwrite("parent plus is "),
    %io:fwrite(packer:pack(ParentPlus)),
    %io:fwrite("\n"),
    true = is_record(ParentPlus, block_plus),
    Difficulty = next_difficulty(ParentPlus),
    PrevPlus = read(PH),
    Prev = block(PrevPlus),
    MB = Block#block.mines_block,
    true = (Block#block.height-1) == Prev#block.height,
    {CH, AH} = {Block#block.channels, Block#block.accounts},
    {CR, AR} = absorb_txs(PrevPlus, MB, Block#block.height, Block#block.txs),
    CH = channel:root_hash(CR),
    AH = account:root_hash(AR),
    MyAddress = keys:address(),
    case MB of
	{ID, MyAddress} ->
	    keys:update_id(ID);
	    %because of hash_check, this function is only run once per block. 
	_ -> ok
    end,
    #block_plus{block = PowBlock, channels = CR, accounts = AR, accumulative_difficulty = next_acc(PrevPlus, Block#block.difficulty), prev_hashes = prev_hashes(hash(Prev))}.

-spec binary_to_file(binary()) -> [1..255,...].
binary_to_file(B) ->
    C = base58:binary_to_base58(B),
    H = C,
    "blocks/"++H++".db".
-spec read(binary()) -> any().
read(Hash) ->
    BF = binary_to_file(Hash),
    Z = db:read(BF),
    case Z of
	[] -> empty;
	A -> binary_to_term(zlib:uncompress(A))
    end.
  
-spec lg(pos_integer()) -> non_neg_integer().
lg(X) ->
    true = X > 0,
    true = is_integer(X),
    lgh(X, 0).
-spec lgh(integer(),non_neg_integer()) -> non_neg_integer().
lgh(1, X) -> X;
lgh(N, X) -> lgh(N div 2, X+1).
-spec read_int(number()) -> tuple().
read_int(N) ->%currently O(n), needs to be improved to O(lg(n))
    true = N >= 0,
    read_int(N, top:doit()).
-spec read_int(number(),binary()) -> tuple().
read_int(N, BH) ->
    Block = read(BH),
    M = height(Block),
    D = M-N,
    if 
	D<0 -> io:fwrite("D is "),
	       io:fwrite(integer_to_list(D)),
	       D = 5;
	D == 0 -> Block;
	true ->
	    read_int(N, prev_hash(lg(D), Block))
    end.
	    
    
    


-spec test() -> 'success'.
test() ->
    io:fwrite("top, \n"),
    block:read(top:doit()),
    PH = top:doit(),
    BP = read(PH),
    Accounts = accounts(BP),
    %Accounts = BP#block_plus.accounts,
    _ = account:get(1, Accounts),
    %{block_plus, Block, _, _, _} = make(PH, [], 1),
    Block = make(PH, [], 1),
    io:fwrite(packer:pack(Block)),
    io:fwrite("top 2, \n"),
    MBlock = mine(Block, 100000000),
    io:fwrite("top 3, \n"),
    check2(MBlock),
    success.
-spec new_id(1) -> pos_integer().
new_id(N) -> 
    {Accounts, _, _, _} = tx_pool:data(),
    new_id(N, Accounts).
-spec new_id(pos_integer(),_) -> pos_integer().
new_id(N, Accounts) ->
   case account:get(N, Accounts) of
       {_, empty, _} -> N;
       _ -> new_id(N+1, Accounts)
   end.
	   
-spec mine_test() -> none().
mine_test() ->
    PH = top:doit(),
    {block_plus, Block, _, _, _} = make(PH, [], keys:id()),
    PBlock = mine(Block, 1000000000),
    block_absorber:doit(PBlock),
    mine_blocks(10, 100000),
    success.
%mine_blocks(N) ->
%    mine_blocks(N, 1000000).
   
-spec mine_blocks(non_neg_integer(),_) -> 'success'.
mine_blocks(0, _) -> success;
mine_blocks(N, Times) -> 
    PH = top:doit(),
    {_,_,_,Txs} = tx_pool:data(),
    ID = case {keys:pubkey(), keys:id()} of
	     {[], X} -> 
		 %io:fwrite("you need to make an account before you can mine. look at docs/new_account.md"),
			X = 294393793232;
	     {_, -1} ->
		 NewID = new_id(1),
		 {NewID, keys:address()};
	     {_, Identity} -> Identity
	 end,
    {block_plus, Block, _, _, _, _} = make(PH, Txs, ID),
    
    %io:fwrite("mining attempt #"),
    %io:fwrite(integer_to_list(N)),
    %io:fwrite(" time "),
   % io:fwrite(integer_to_list(time_now())),
   % io:fwrite(" diff "),
    %io:fwrite(integer_to_list(Block#block.difficulty)),
    %erlang:system_info(logical_processors_available)
    Cores = guess_number_of_cpu_cores(),
    %io:fwrite(" using "),
    %io:fwrite(integer_to_list(Cores)),
    %io:fwrite(" CPU"),
    %io:fwrite("\n"),
    F = fun() ->
		case mine(Block, Times) of
		    false -> false;
		    PBlock -> 
			io:fwrite("FOUND A BLOCK !\n"),
			block_absorber:doit(PBlock)
		end
	end,
    spawn_many(Cores, F),
    F(),
    mine_blocks(N-1, Times).
    
-spec spawn_many(non_neg_integer(),fun(() -> 'false' | 'ok')) -> 'ok'.
spawn_many(0, _) -> ok;
spawn_many(N, F) -> 
    spawn(F),
    spawn_many(N-1, F).
-spec guess_number_of_cpu_cores() -> integer().
guess_number_of_cpu_cores() ->    
    X = erlang:system_info(logical_processors_available),
    if
        X == unknown ->
	    % Happens on Mac OS X.
            erlang:system_info(schedulers);
	is_integer(X) -> 
	    %ubuntu
	    X;
	true -> io:fwrite("number of CPU unknown, only using 1"), 1
    end. 
