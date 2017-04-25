-module(tx_pool).
-behaviour(gen_server).
%this module holds the txs ready for the next block.
%It needs to use txs:digest to keep track of the Accounts and Channels dicts. This module needs to be ready to share either of those dicts.
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
absorb/4,absorb_tx/3,dump/0,data/0,test/0]).
-record(f, {txs = [], accounts = 1, channels = 0, height = 0}).%block:genesis only stores a single account, so the first time account was updated should be a 1.
-spec init('ok') -> {'ok',#f{txs::[]}}.
init(ok) -> 
    io:fwrite("tx pool started\n"),
    %process_flag(trap_exit, true),
    F = state_now(),
    {ok, F}.
-spec state_now() -> #f{txs::[]}.
state_now() ->
    B = block:read(top:doit()),
    #f{accounts = block:accounts(B), channels = block:channels(B), height = block:height(B)}.
    
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, _) -> io:format("tx pool died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(_, X) -> {noreply, X}.
-spec handle_call('data' | 'dump' | {'absorb_tx',_,_,_} | {'absorb',_,_,_,_},_,_) -> {'reply',0 | {_,_,_,[any()]},#f{}}.
handle_call(dump, _From, _) -> {reply, 0, state_now()};
handle_call({absorb_tx, NewChannels, NewAccounts, Tx}, _From, F) ->
    NewTxs = [Tx|F#f.txs],
    B = size(term_to_binary(NewTxs)),
    MBS = constants:max_block_size(),
    FinalTxs = if
	B > MBS -> F#f.txs;
	true -> NewTxs
    end,
    {reply, 0, F#f{txs = FinalTxs, channels = NewChannels, accounts = NewAccounts}}; 
handle_call({absorb, NewChannels, NewAccounts, Txs, Height}, _From, _) ->
    {reply, 0, #f{txs = Txs, accounts = NewAccounts, channels = NewChannels, height = Height}};
handle_call(data, _From, F) -> {reply, {F#f.accounts, F#f.channels, F#f.height, flip(F#f.txs)}, F}.
-spec data() -> any().
data() -> gen_server:call(?MODULE, data). %{accounts, channels, height, txs}
-spec dump() -> any().
dump() -> gen_server:call(?MODULE, dump).
-spec absorb_tx(_,_,_) -> any().
absorb_tx(Channels, Accounts, Tx) -> gen_server:call(?MODULE, {absorb_tx, Channels, Accounts, Tx}).
-spec absorb(_,_,_,_) -> any().
absorb(Channels, Accounts, Txs, Height) ->
    gen_server:call(?MODULE, {absorb, Channels, Accounts, Txs, Height}).
-spec flip([any()]) -> [any()].
flip(X) -> flip(X, []).
-spec flip([any()],[any()]) -> [any()].
flip([], A) -> A;
flip([H|T], A) -> flip(T, [H|A]).
    
-spec test() -> 'success'.
test() ->
    success.
