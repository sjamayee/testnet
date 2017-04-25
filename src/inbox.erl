%This module keeps track of messages you receive.
-module(inbox).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, peers/0,msg_ids/1,read/2,delete/2,delete/1,get/1,get_helper/2,test/0]).
-record(f, {next = 0, msgs = dict:new()}).
-spec init('ok') -> {'ok',dict:dict(_,_)}.
init(ok) -> {ok, dict:new()}.
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec terminate(_,_) -> 'ok'.
terminate(_, _) -> io:format("died!"), ok.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info(_, X) -> {noreply, X}.
-spec handle_cast({'delete',_} | {'delete',_,_} | {'get',_,binary() | tuple()},dict:dict(_,_)) -> {'noreply',dict:dict(_,_)}.
handle_cast({delete, Id}, X) -> 
    NewX = case dict:find(Id, X) of
               error -> X;
               {ok, _} ->
                   dict:erase(Id, X)
           end,
    {noreply, NewX};
handle_cast({delete, Id, Index}, X) -> 
    NewX = case dict:find(Id, X) of
        error -> X;
        {ok, A} ->
            case dict:find(Index, A#f.msgs) of
                error -> X;
                {ok, _} ->
                    C = dict:erase(Index, A#f.msgs),
                    dict:store(Id, #f{msgs = C, next = A#f.next}, X)
            end
    end,
    {noreply, NewX};
handle_cast({get, Id, Msg}, X) ->
    F = case dict:find(Id, X) of
            error -> #f{};
            {ok, Z} -> Z
        end,
    N = F#f.next,
    D = dict:store(N, Msg, F#f.msgs),
    E = dict:erase(N-free_constants:inbox_per_peer(), D),
    S = size(Msg),
    true = S < free_constants:max_message_size(),
    NewX = dict:store(Id, #f{next = N+1, msgs = E}, X),
    {noreply, NewX}.
-spec handle_call('peers' | {'msg_ids',_} | {'read',_,_},_,dict:dict(_,_)) -> {'reply',_,dict:dict(_,_)}.
handle_call(peers, _From, X) -> 
    {reply, dict:fetch_keys(X), X};
handle_call({msg_ids, Id}, _From, X) -> 
    B = case dict:find(Id, X) of
            error -> no_peer;
            {ok, A} -> dict:fetch_keys(A#f.msgs)
        end,
    {reply, B, X};
handle_call({read, Id, Index}, _From, X) -> 
    B = case dict:find(Id, X) of
        error -> no_peer;
        {ok, A} -> 
                case dict:find(Index, A#f.msgs) of
                    error -> no_message;
                    {ok, C} -> C
                end
        end,
    {reply, B, X}.

-spec delete(_,_) -> 'ok'.
delete(Id, Index) -> gen_server:cast(?MODULE, {delete, Id, Index}).
-spec delete(_) -> 'ok'.
delete(Id) -> gen_server:cast(?MODULE, {delete, Id}).
-spec get(_) -> 'ok'.
get(Msg) -> 
    M = encryption:get_msg(Msg),
    FromId = encryption:id(M),
    EM = encryption:msg(M),
    B = << <<"~>">>/binary, EM/binary >>,
    get_helper(FromId, B).
-spec get_helper(_,_) -> 'ok'.
get_helper(From, Message) -> gen_server:cast(?MODULE, {get, From, Message}).
-spec peers() -> any().
peers() -> gen_server:call(?MODULE, peers).
-spec msg_ids(_) -> <<_:56>> | [any()].
msg_ids(Id) -> merge_sort(gen_server:call(?MODULE, {msg_ids, Id})).
-spec read(_,_) -> any().
read(Id, Index) -> gen_server:call(?MODULE, {read, Id, Index}).
-spec merge_sort('no_peer' | [any()]) -> <<_:56>> | [any()].
merge_sort(no_peer) -> <<"no peer">>;
merge_sort(L) -> ms2(L, []).
-spec ms2([any()],[[any(),...]]) -> [any()].
ms2([], Out) ->  ms3(Out);
ms2([H|T], Out) -> ms2(T, [[H]|Out]).
-spec ms3([[any()]]) -> [any()].
ms3(X) when length(X) == 1 -> hd(X);
ms3(X) -> ms3(ms4(X, [])).
-spec ms4([[any()]],[[any()]]) -> [[any()]].
ms4([], Out) -> Out;
ms4([X|[]], Out) -> [X|Out];
ms4([A|[B|X]], Out) -> ms4(X, [merge(A, B)|Out]).
-spec merge([any()],[any()]) -> [any()].
merge(X, Y) -> merge(flip(X), flip(Y), []).
-spec merge([any()],[any()],[any()]) -> [any()].
merge([], [], Out) -> Out;
merge([], [Y|Yt], Out) -> merge([], Yt, [Y|Out]);
merge([X|Xt], [], Out) -> merge(Xt, [], [X|Out]);
merge([X|Xt], [Y|Yt], Out) when X > Y -> merge(Xt, [Y|Yt], [X|Out]);
merge([X|Xt], [Y|Yt], Out) -> merge([X|Xt], Yt, [Y|Out]).
-spec flip([any()]) -> [any()].
flip(X) -> flip(X, []).
-spec flip([any()],[any()]) -> [any()].
flip([], Out) -> Out;
flip([H|T], Out) -> flip(T, [H|Out]).
-spec test() -> 'success'.
test() ->
    Peer = 1,
    Sorted = [1,2,3,4,5,6,7,8,9],
    Sorted = merge_sort([3,6,5,2,4,8,7,9,1]),    
    get_helper(Peer, <<"hello">>),
    get_helper(Peer, <<"hello2">>),
    get_helper(Peer, <<"hello3">>),
    P = peers(),
    P = [Peer],
    X = msg_ids(Peer),
    X = [0,1,2],
    H = read(Peer, 0),
    H = <<"hello">>,
    delete(Peer, 0),
    Y = msg_ids(Peer),
    Y = [1,2],
    delete(Peer),
    P2 = peers(),
    P2 = [],
    success.
                    
    
    
    
    
