-module(easy).
-compile(export_all).

-define(Fee, free_constants:tx_fee()).

-spec sync() -> pid().
sync() ->
    spawn(fun() -> sync3() end).
-spec sync3() -> 'ok' | pid().
sync3() ->
    Height = block:height(block:read(top:doit())),
    download_blocks:sync_all(peers:all(), Height),
    sync2(Height, 600).
-spec sync2(_,_) -> 'ok' | pid().
sync2(_Height, 0) -> ok;
sync2(Height, N) ->
    timer:sleep(100),
    Height2 = block:height(block:read(top:doit())),
    if
	Height2 > Height -> 
	    timer:sleep(1400),
	    sync();
	true -> sync2(Height, N-1)
   end. 
   
-spec tx_maker(fun((_,_) -> any())) -> 'ok'.
tx_maker(F) -> 
    {Accounts, Channels,_,_} = tx_pool:data(),
    {Tx, _} = F(Accounts, Channels),
    case keys:sign(Tx, Accounts) of
	{error, locked} -> ok;
	Stx -> tx_pool_feeder:absorb(Stx)
    end.
-spec create_account(_,_,_) -> 'ok'.
create_account(NewAddr, Amount, ID) ->
    create_account(NewAddr, Amount, ?Fee, ID).
-spec create_account(_,_,_,_) -> 'ok'.
create_account(NewAddr, Amount, Fee, ID) ->
    F = fun(Accounts, _) ->
		create_account_tx:make(NewAddr, to_int(Amount), Fee, keys:id(), ID, Accounts) end,
    tx_maker(F).
-spec spend(_,_) -> 'ok'.
spend(ID, Amount) ->
    K = keys:id(),
    if 
	ID == K -> io:fwrite("you can't spend money to yourself\n");
	true -> 
	    A = to_int(Amount),
	    spend(ID, A, ?Fee)
    end.
-spec spend(_,_,_) -> 'ok'.
spend(ID, Amount, Fee) ->
    F = fun(Accounts, _) ->
		spend_tx:make(ID, Amount, Fee, keys:id(), Accounts) end,
    tx_maker(F).
    
-spec delete_account(_,_) -> 'ok'.
delete_account(ID, Fee) ->
    F = fun(Accounts, _) ->
		delete_account_tx:make(keys:id(), ID, Fee, Accounts) end,
    tx_maker(F).

-spec repo_account(_,_) -> 'ok'.
repo_account(ID, Fee) ->   
    F = fun(Accounts, _) ->
		repo_tx:make(ID, Fee, keys:id(), Accounts) end,
    tx_maker(F).
-spec new_channel(number(),number()) -> any().
new_channel(Bal1, Bal2) ->
    new_channel(Bal1, Bal2, ?Fee, 10).
-spec new_channel(number(),number(),_,_) -> any().
new_channel(Bal1, Bal2, Fee, Delay) ->
    {_,Channels, _,_} = tx_pool:data(),
    CID = new_channel2(1, Channels),
    B1 = to_int(Bal1),
    B2 = to_int(Bal2),
    new_channel(constants:server_ip(), 
		constants:server_port(), 
		CID, B1, B2, 0, Fee, Delay).
-spec new_channel2(number(),_) -> number().
new_channel2(ID, Channels) ->
    <<X:8>> = crypto:strong_rand_bytes(1),
    case channel:get(ID+X, Channels) of
	{_, empty, _} -> ID+X;
	X -> new_channel2(ID+256, Channels)
    end.
-spec new_channel(_,_,_,_,_,_,_,_) -> any().
new_channel(IP, Port, CID, Bal1, Bal2, Rent, Fee, Delay) ->
    internal_handler:doit({new_channel, IP, Port, CID, Bal1, Bal2, Rent, Fee, Delay}).
-spec channel_balance() -> [[any()] | char()].
channel_balance() ->
    I = integer_channel_balance(),
    pretty_display(I).
-spec integer_channel_balance() -> number().
integer_channel_balance() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    {ok, CD} = channel_manager:read(Other),
    SSPK = channel_feeder:them(CD),
    SPK = testnet_sign:data(SSPK),
    SS = channel_feeder:script_sig_them(CD),
    {Accounts, Channels, NewHeight, _Txs} = tx_pool:data(),
    {Amount, _} = spk:run(fast, SS, SPK, NewHeight, 0, Accounts, Channels),
    CID = spk:cid(SPK),
    {_, Channel, _} = channel:get(CID, Channels),
    channel:bal1(Channel)-Amount.
-spec dice(number()) -> any().
dice(Amount) ->
    unlocked = keys:status(),
    A = to_int(Amount),
    internal_handler:doit({dice, A, constants:server_ip(), constants:server_port()}).
-spec close_channel() -> any().
close_channel() ->
    internal_handler:doit({close_channel, constants:server_ip(), constants:server_port()}).
-spec solo_close_channel() -> any().
solo_close_channel() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    internal_handler:doit({channel_solo_close, Other}).
-spec channel_timeout() -> 'ok'.
channel_timeout() ->
    {ok, Other} = talker:talk({id}, constants:server_ip(), constants:server_port()),
    Fee = free_constants:tx_fee(),
    {Accounts,Channels,_,_} = tx_pool:data(),
    {ok, CD} = channel_manager:read(Other),
    CID = channel_feeder:cid(CD),
    {Tx, _} = channel_timeout:make(keys:id(), Accounts, Channels, CID, Fee),
    Stx = keys:sign(Tx, Accounts),
    tx_pool_feeder:absorb(Stx).
    
-spec to_int(number()) -> integer().
to_int(X) ->
    round(X * constants:token_decimals()).

-spec grow_channel(_,_,_,_,_) -> 'ok'.
grow_channel(CID, Bal1, Bal2, Rent, Fee) ->
    F = fun(Accounts, Channels) ->
		grow_channel_tx:make(CID, Accounts, Channels, Bal1, Bal2, Rent, Fee) end,
    tx_maker(F).

-spec channel_team_close(_,_,_) -> 'ok'.
channel_team_close(CID, Amount, Fee) ->
    F = fun(Accounts, Channels) ->
		channel_team_close_tx:make(CID, Accounts, Channels, Amount, Fee) end,
    tx_maker(F).

-spec channel_repo(_,_) -> 'ok'.
channel_repo(CID, Fee) ->
    F = fun(Accounts, Channels) ->
		channel_repo_tx:make(keys:id(), CID, Fee, Accounts, Channels) end,
    tx_maker(F).

-spec channel_solo_close(_,_,_,_) -> 'ok'.
channel_solo_close(CID, Fee, SPK, ScriptSig) ->
    F = fun(Accounts, Channels) ->
		channel_solo_close:make(keys:id(), CID, Fee, SPK, ScriptSig, Accounts, Channels) end,
    tx_maker(F).

-spec channel_timeout(_,_) -> 'ok'.
channel_timeout(CID, Fee) ->
    F = fun(Accounts, Channels) ->
		channel_timeout_tx:make(keys:id(), Accounts, Channels, CID, Fee) end,
    tx_maker(F).

-spec channel_slash(_,_,_,_) -> 'ok'.
channel_slash(CID, Fee, SPK, SS) ->
    F = fun(Accounts, Channels) ->
		channel_slash_tx:make(keys:id(), CID, Fee, SPK, SS, Accounts, Channels) end,
    tx_maker(F).

-spec account(number()) -> {'acc',_,char(),_,_,_}.
account(ID) ->
    {Accounts, _,_,_} = tx_pool:data(),
    case account:get(ID, Accounts) of
	{_,empty,_} ->
	    io:fwrite("this account does not yet exist\n"),
	    account:new(-1,0,0,0);
	{_, A, _} -> A
    end.

-spec account() -> {'acc',_,char(),_,_,_}.
account() -> account(keys:id()).
-spec integer_balance() -> any().
integer_balance() -> account:balance(account()).
-spec balance() -> [[any()] | char()].
balance() ->
    I = integer_balance(),
    pretty_display(I).
-spec pretty_display(number()) -> [[any()] | char()].
pretty_display(I) ->
    F = I / constants:token_decimals(),
    io_lib:format("~.8f", [F]).
-spec off() -> none().
off() -> testnet_sup:stop().

%mine() ->
%    mine:start().
    %mine(10000000000).
%mine(N) -> 
    %sync(),
    %block:mine_blocks(N, 100000, 30). 
%second number is how many nonces we try per round.
%first number is how many rounds we do.
