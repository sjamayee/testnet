-module(port).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, check/0,change/1]).
-spec init('ok') -> {'ok',8040}.

init(ok) -> {ok, constants:default_port()}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.

terminate(_, _) -> io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.

handle_info(_, X) -> {noreply, X}.
-spec handle_cast({'change',_},_) -> {'noreply',_}.

handle_cast({change, X}, _) -> 
    {noreply, X}.
-spec handle_call('check',_,_) -> {'reply',_,_}.

handle_call(check, _From, X) -> {reply, X, X}.

-spec check() -> any().

check() -> gen_server:call(?MODULE, check).
-spec change(_) -> any().

change(X) -> 
    io:fwrite("changing port!!!\n"),
    io:fwrite("changing port!!!\n"),
    if
        not is_integer(X) -> change(list_to_integer(X));
        true ->
            true = X > 0,
            gen_server:cast(?MODULE, {change, X}),
            X = check()
    end.
