-module(testnet_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(_,_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start(_StartType, _StartArgs) ->
    ssl:start(),
    application:start(inets),
    testnet_sup:start_link().


-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

    
