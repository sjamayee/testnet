-module(peers).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, 
	 update/4,all/0,add/2,add/1,read/2,remove/2,
	update_score/3, initial_score/0,
	cid/1,set_cid/3]).    
-record(r, {height =0, hash=0, cid, score=100000}).%lower score is better.
-spec cid(#r{}) -> any().

cid(X) -> X#r.cid.
-spec init('ok') -> {'ok',_}.

init(ok) -> {ok, default_peers()}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.

terminate(_, _) -> io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.

handle_info(_, X) -> {noreply, X}.
-spec handle_cast({'add',_,_} | {'remove',_,_} | {'score',_,_,_} | {'set_cid',_,_,_} | {'update',_,_,_,_},_) -> {'noreply',_}.

handle_cast({remove, IP, Port}, X) -> 
    K = key(IP, Port),
    NewX = dict:remove(K, X),
    {noreply, NewX};
handle_cast({add, IP, Port}, X) -> 
    NewX = load_peers([{IP, Port}], X),
    {noreply, NewX};
handle_cast({score, IP, Port, N}, X) ->
    K = key(IP, Port),
    {ok, Old} = dict:find(K, X),
    %A = Old#r.score,
    %B = (A*99+N) div 100,
    NewR = Old#r{score = N},
    NewX = dict:store(K, NewR, X),
    {noreply, NewX};
handle_cast({set_cid, IP, Port, CID}, X) ->
    K = key(IP, Port),
    {ok, Old} = dict:find(K, X),
    New = Old#r{cid = CID},
    NewX = dict:store(K, New, X),
    {noreply, NewX};
handle_cast({update, IP, Port, Height, Hash}, X) ->
    K = key(IP, Port),
    {ok, Old} = dict:find(K, X),
    NewX = if
	       Height > Old#r.height ->
		   A = Old#r{height = Height, hash = Hash},
		   dict:store(K, A, X);
	       true ->
		   X
	   end,
    {noreply, NewX}.
-spec handle_call('all' | {'read',_,_},_,dict:dict(_,_)) -> {'reply',_,dict:dict(_,_)}.

handle_call(all, _From, X) ->
    {reply, dict:fetch_keys(X), X};
handle_call({read, IP, Port}, _From, X) -> 
    K = key(IP, Port),
    O = case dict:find(K, X) of
        error -> <<"none">>;
        {ok, Val} -> Val
    end,
    {reply, O, X}.
-spec set_cid(_,_,_) -> 'ok'.

set_cid(IP, Port, CID) ->
    gen_server:cast(?MODULE, {set_cid, IP, Port, CID}).
-spec key(_,_) -> {_,_}.

key(IP, Port) -> {IP, Port}.
-spec all() -> any().

all() -> gen_server:call(?MODULE, all).
-spec add([[any(),...] | {[any()] | tuple(),_}]) -> 'ok'.

add([]) -> ok;
add([[IP, Port]|T]) ->
    add(IP, Port),
    add(T);
add([{IP, Port}|T]) -> 
    add(IP, Port),
    add(T).
-spec add([any()] | tuple(),_) -> 'ok'.

add(IP, Port) -> 
    NIP = if
	      is_tuple(IP) -> IP;
	      is_list(IP) -> list_to_tuple(IP)
	  end,
    gen_server:cast(?MODULE, {add, NIP, Port}).
-spec update_score(_,_,_) -> 'ok'.

update_score(IP, Port, N) ->
    gen_server:cast(?MODULE, {score, IP, Port, N}).

-spec update(_,_,_,_) -> 'ok'.

update(IP, Port, Height, Hash) ->
    gen_server:cast(?MODULE, {update, IP, Port, Height, Hash}).
-spec remove(_,_) -> 'ok'.

remove(IP, Port) -> gen_server:cast(?MODULE, {remove, IP, Port}).
-spec read(_,_) -> any().

read(IP, Port) -> gen_server:call(?MODULE, {read, IP, Port}).

-spec default_peers() -> any().

default_peers() -> 
    D = constants:peers(),
    load_peers(D, dict:new()).
-spec load_peers([{_,_}],_) -> any().

load_peers([], D) -> D;
load_peers([{IP, Port}|T], Dict) ->
    K = key(IP, Port),
    %don't re-write the same peer twice.
    D2 = case dict:find(K, Dict) of
	     error ->
		 dict:store(K, #r{}, Dict);
	     _ -> Dict
	 end,
    load_peers(T, D2).

-spec initial_score() -> 100000.

initial_score() -> 100000.
