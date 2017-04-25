-module(mine).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,
	 handle_info/2,init/1,terminate/2,
	 start/0,stop/0,is_on/0]).
-spec init('ok') -> {'ok','stop'}.
init(ok) -> {ok, stop}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, _) -> io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(mine, go) ->
    spawn(fun() ->
		  block:mine_blocks(10, 50000),
		  mine() end),
    spawn(fun() -> easy:sync() end),
    {noreply, go};
handle_cast(start, _) ->
    Cores = block:guess_number_of_cpu_cores(),
    io:fwrite("start mining with "),
    io:fwrite(integer_to_list(Cores)),
    io:fwrite(" cores.\n"),
    {noreply, go};
handle_cast(stop, _) ->
    {noreply, stop};
handle_cast(_, X) -> {noreply, X}.
-spec handle_call(_,_,_) -> {'reply',_,_}.
handle_call(status, _From, X) -> {reply, X, X};
handle_call(_, _From, X) -> {reply, X, X}.

-spec start() -> 'ok'.
start() ->
    gen_server:cast(?MODULE, start),
    timer:sleep(100),
    mine().
-spec mine() -> 'ok'.
mine() ->
    gen_server:cast(?MODULE, mine).

-spec stop() -> 'ok'.
stop() -> gen_server:cast(?MODULE, stop).
-spec is_on() -> any().
is_on() ->
    gen_server:call(?MODULE, status).
