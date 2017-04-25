-module(tx_pool_feeder).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, absorb/1]).
-spec init('ok') -> {'ok',[]}.
init(ok) -> {ok, []}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, _) -> io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast({absorb, SignedTx}, X) ->
    Tx = testnet_sign:data(SignedTx),
    Fee = element(4, Tx),
    true = Fee > free_constants:minimum_tx_fee(),
    {Accounts, Channels, Height, Txs} = tx_pool:data(),
    true = testnet_sign:verify(SignedTx, Accounts),
    B = is_in(SignedTx, Txs), %this is very ugly. once we have a proper CLI we can get rid of this crutch.
    if
	B -> ok;
	true ->
	    {NewChannels, NewAccounts} = 
		txs:digest([SignedTx], Channels, Accounts, Height+1),
	    tx_pool:absorb_tx(NewChannels, NewAccounts, SignedTx)
    end,
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
-spec handle_call(_,_,_) -> {'reply',_,_}.
handle_call(_, _From, X) -> {reply, X, X}.
    
-spec absorb(_) -> 'ok'.
absorb(SignedTx) -> 
    gen_server:cast(?MODULE, {absorb, SignedTx}).

-spec is_in({'signed',tuple(),binary() | [1..255],binary() | [1..255],_,_,_},maybe_improper_list()) -> boolean().
is_in(A, [A|_]) -> true;
is_in(_, []) -> false;
is_in(A, [_|T]) -> is_in(A, T).
