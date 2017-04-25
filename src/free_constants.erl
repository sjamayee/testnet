-module(free_constants).
%These constants can be different on every node in the network. You can adjust these variables to suit your own situation.
-compile(export_all).
-spec hashlock_time() -> 30.
hashlock_time() -> 30.
-spec channel_delay() -> 100.
channel_delay() ->
    100.
-spec max_channel() -> integer().
max_channel() -> constants:initial_coins() div 100000.
-spec max_message_size() -> 10000.
max_message_size() -> 10000.
-spec inbox_per_peer() -> 100.
inbox_per_peer() -> 100.
-spec liquidity_ratio() -> {'f',_,_}.
liquidity_ratio() -> fractions:new(2, 3).%if a user is willing to put 100 coins into a channel, then the server is willing to put 200 in.
-spec tx_fee() -> 10.
tx_fee() -> %when you make a tx, this is the fee you spend by default. 
    10.
-spec minimum_tx_fee() -> integer().
minimum_tx_fee() ->%only txs with this fee or higher get accepted into your mempool. If you are a miner, you are censoring all txs with lower fees.
    constants:initial_coins() div 1000000000000.
-spec fork_tolerance() -> 20.
fork_tolerance() ->    
   %this is how long of a fork we can recover from. If this number is bigger, it takes longer to sync with the network because you download more unnecessary blocks.
    %It is best to keep this number low when you first sync, and make it bigger once you are synced with the network.
    %on nodes that are mining, this should probably be set very low. 
    20.
-spec min_channel_ratio() -> float().
min_channel_ratio() ->
    %So the customer needs to put in twice as much money as the server.
    0.5.
    %{f, 1, 2}.
-spec bets() -> [{'dice',[46 | 47 | 98 | 99 | 100 | 101 | 102 | 105 | 114 | 115 | 116,...]},...].
bets() -> %tuple list. {Name, BetFile}
    [
     {dice, "src/bets/dice.fs"}
    ].
-spec gas_limit() -> 1000000.
gas_limit() ->
    constants:gas_limit().
-spec time_limit() -> 100000.
time_limit() ->
    %maximum number of miliseconds to wait for a channel contract to process.
    %if this number is too high, then it is easy to
    100000.
-spec space_limit() -> 100000.
space_limit() ->
    100000.
    
-spec vm(_,_) -> any().
vm(SS, State) ->
    chalang:vm(SS, time_limit(), space_limit(), constants:fun_limit(), constants:var_limit(), State).

-spec min_channel_delay() -> 4.
min_channel_delay() -> 4.
-spec max_channel_delay() -> 100.
max_channel_delay() -> 100.

