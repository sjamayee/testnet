-module(block_absorber).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
	 handle_cast/2,handle_info/2,init/1,terminate/2,
	 doit/1, save_helper/1]).
-spec init('ok') -> {'ok',[]}.
init(ok) -> 
    %save(block:genesis()),
    {ok, []}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, _) -> io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast({doit, BP}, X) -> 
    absorb(BP),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
-spec handle_call(_,_,_) -> {'reply',_,_}.
handle_call(_, _From, X) -> {reply, X, X}.

-spec doit(_) -> 'ok'.
doit(X) ->
    gen_server:cast(?MODULE, {doit, X}).
    
-spec absorb(tuple()) -> any().
absorb(BP) ->
    BH = block:hash(BP),
    case block_hashes:check(BH) of
	true -> ok;%If we have seen this block before, then don't process it again.
	false ->
	    block_hashes:add(BH),%Don't waste time checking invalid blocks more than once.
	    io:fwrite("absorb block "),
	    io:fwrite(packer:pack(BP)),
	    io:fwrite("\n"),
	    BP2 = block:check2(BP),
	    save(BP2)
    end.   
-spec save_helper(tuple()) -> any().
save_helper(BlockPlus) ->
    Z = zlib:compress(term_to_binary(BlockPlus)),
    binary_to_term(zlib:uncompress(Z)),%sanity check, not important for long-term.
    %Hash = testnet_hasher:doit(BlockPlus),
    Hash = block:hash(BlockPlus),
    BF = block:binary_to_file(Hash),
    db:save(BF, Z).
    
-spec save({'block_plus',tuple(),_,_,number(),tuple()}) -> any().
save(BlockPlus) ->
    save_helper(BlockPlus),
    top:add(BlockPlus),
    block:hash(BlockPlus).
