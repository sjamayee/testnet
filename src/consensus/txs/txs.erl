-module(txs).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, dump/0,txs/0,digest/4]).
-spec init('ok') -> {'ok',[]}.
init(ok) -> {ok, []}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, _) -> io:format("txs died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_call('txs',_,_) -> {'reply',_,_}.
handle_call(txs, _From, X) -> {reply, X, X}.
-spec handle_cast('dump' | {'add_tx',_},_) -> {'noreply',maybe_improper_list()}.
handle_cast(dump, _) -> {noreply, []};
handle_cast({add_tx, Tx}, X) -> {noreply, [Tx|X]}.
-spec dump() -> 'ok'.
dump() -> gen_server:cast(?MODULE, dump).
-spec txs() -> any().
txs() -> gen_server:call(?MODULE, txs).
-spec digest([any()],_,_,_) -> {_,_}.
digest([], Channels, Accounts, _) -> {Channels, Accounts};
digest([SignedTx|Txs], Channels, Accounts, NewHeight) ->
    true = testnet_sign:verify(SignedTx, Accounts),
    Tx = testnet_sign:data(SignedTx),
    {NewChannels, NewAccounts} = digest2(Tx, Channels, Accounts, NewHeight),
    digest(Txs, NewChannels, NewAccounts, NewHeight).
-spec digest2(tuple(),_,_,_) -> 2 | {_,_}.
digest2(A, B, C, D) ->
    case element(1, A) of
	ca -> create_account_tx:doit(A, B, C, D);
	spend -> spend_tx:doit(A, B, C, D);
	da -> delete_account_tx:doit(A, B, C, D);
	repo -> repo_tx:doit(A, B, C, D);
	nc -> new_channel_tx:doit(A, B, C, D);
	gc -> grow_channel_tx:doit(A, B, C, D);
	ctc -> channel_team_close_tx:doit(A,B,C,D);
	cr -> channel_repo_tx:doit(A,B,C,D);
	csc -> channel_solo_close:doit(A,B,C,D);
	timeout -> channel_timeout_tx:doit(A, B, C, D);
	cs -> channel_slash_tx:doit(A,B, C, D);
	X -> X=2
    end.
 
