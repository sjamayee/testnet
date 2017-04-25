-module(channel_manager).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 keys/0,read/1,delete/1,write/2]).
-define(LOC, constants:channel_manager()).
-spec init('ok') -> {'ok',_}.
init(ok) -> 
    X = db:read(?LOC),
    Ka = if
	     X == "" -> dict:new();
	     true -> X
	 end,
    %process_flag(trap_exit, true),
    {ok, Ka}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, K) -> 
    db:save(?LOC, K),
    io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast({write, CID, Data}, X) -> 
    NewX = dict:store(CID, Data, X),
    db:save(?LOC, NewX),
    %this db:save is only for power failures. Without it, you could lose channel data on power failure. This line can be removed to make the node update channels faster.
    {noreply, NewX};
handle_cast({delete, CID}, X) -> 
    NewX = dict:delete(CID, X),
    db:save(?LOC, NewX),
    %this db:save is only for power failures. Without it, you could lose channel data on power failure. This line can be removed to make the node update channels faster.
    {noreply, NewX};
handle_cast(_, X) -> {noreply, X}.
-spec handle_call(_,_,_) -> {'reply',_,_}.
handle_call(keys, _From, X) ->
    {reply, dict:fetch_keys(X), X};
handle_call({read, CID}, _From, X) -> 
    {reply, dict:find(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

-spec read(_) -> any().
read(CID) -> gen_server:call(?MODULE, {read, CID}).
-spec keys() -> any().
keys() -> gen_server:call(?MODULE, keys).
-spec delete(_) -> 'ok'.
delete(CID) -> gen_server:cast(?MODULE, {delete, CID}).
-spec write(_,{'cd',_,_,_,_,_,_,_}) -> 'ok'.
write(CID, Data) -> 
    true = is_list(channel_feeder:script_sig_them(Data)),
    true = is_list(channel_feeder:script_sig_me(Data)),
    gen_server:cast(?MODULE, {write, CID, Data}).

