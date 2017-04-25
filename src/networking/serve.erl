-module(serve).
-export([start/0, start/1, pw/0, pw/1]).
-spec start() -> {'ok',_}.

start() -> start(port:check()).
-spec start(number()) -> {'ok',_}.

start(Port) ->
    io:fwrite("start server\n"),
    
    D_internal = [
	 {'_', [
		{"/:file", main_handler, []},
		{"/", internal_handler, []}
	       ]}
	],
    D = [
	 {'_', [
		{"/:file", external_handler, []},
		{"/", handler, []}
	       ]}
	],
    Dispatch_internal = cowboy_router:compile(D_internal),
    Dispatch = cowboy_router:compile(D),
    K_internal = [
	 {env, [{dispatch, Dispatch_internal}]}
	],
    K = [
	 {env, [{dispatch, Dispatch}]}
	],
    %{ok, _} = cowboy:start_http(http_internal, 100, [{ip, {127,0,0,1}},{port, Port+1}], K_internal),
    {ok, _} = cowboy:start_http(http_internal, 100, [{ip, {0,0,0,0}},{port, Port+1}], K_internal),
    {ok, _} = cowboy:start_http(http, 100, [{ip, {0,0,0,0}},{port, Port}], K).

-spec pw() -> {'ok',_}.

pw() ->  start(port:check()).
-spec pw(_) -> {'ok',_}.

pw(X) ->
    port:change(X),
    pw().

