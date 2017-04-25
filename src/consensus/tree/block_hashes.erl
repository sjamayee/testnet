-module(block_hashes).
%each blockhash is about 12 bytes. We need to prepare for about 10000000 blocks. So that would be 12 megabytes of data. We can keep this all in ram.
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, add/1,check/1,test/0]).
-define(LOC, constants:block_hashes()).
-spec init('ok') -> {'ok',_}.
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    K = if
	    X == "" ->
		i_new();
	    true -> X
	end,
    {ok, K}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("block_hashes died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast({add, H}, X) ->
    N = i_insert(H, X),
    db:save(?LOC, N),%This line is only necessary for power failures
    {noreply, N};
handle_cast(_, X) -> {noreply, X}.
-spec handle_call(_,_,_) -> {'reply',_,_}.
handle_call({check, H}, _From, X) ->
    B = i_check(H, X), %true means it doesn't exist.
    {reply, B, X} ;
handle_call(_, _From, X) -> {reply, X, X}.

-spec add(binary() | tuple()) -> 'ok'.
add(X) -> 
    true = size(X) == constants:hash_size(),
    gen_server:cast(?MODULE, {add, X}).

-spec check(binary() | tuple()) -> any().
check(X) ->
    true = size(X) == constants:hash_size(),
    gen_server:call(?MODULE, {check, X}).

-spec i_new() -> gb_sets:set(_).
i_new() ->
    gb_sets:new().
-spec i_insert(_,gb_sets:set(_)) -> gb_sets:set(_).
i_insert(H, X) ->
    gb_sets:insert(H, X).
-spec i_check(_,gb_sets:set(_)) -> boolean().
i_check(H, X) ->
    gb_sets:is_member(H, X).

-spec test() -> 'success'.
test() ->
    V1 = <<1:92>>,
    V2 = <<2:92>>,
    D = i_new(),
    D2 = i_insert(V1, D),
    false = i_check(V2, D2),
    true = i_check(V1, D2),
    success.
