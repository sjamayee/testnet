-module(main_handler).

-export([init/3, handle/2, terminate/3]).
%example of talking to this handler:
%httpc:request(post, {"http://127.0.0.1:3011/", [], "application/octet-stream", "echo"}, [], []).
%curl -i -d '[-6,"test"]' http://localhost:3011
-spec handle(_,_) -> {'ok',_,binary()}.

handle(Req, _) ->
    {F, _} = cowboy_req:path(Req),
    File = << <<"src/web">>/binary, F/binary>>,
    {ok, _Data, _} = cowboy_req:body(Req),
    Headers = [{<<"content-type">>, <<"text/html">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    Text = read_file(File),
    {ok, Req2} = cowboy_req:reply(200, Headers, Text, Req),
    {ok, Req2, File}.
-spec read_file(binary()) -> binary() | string().

read_file(F) ->
    {ok, File } = file:open(F, [read, binary, raw]),
    {ok, O} =file:pread(File, 0, filelib:file_size(F)),
    file:close(File),
    O.
-spec init(_,_,_) -> {'ok',_,[]}.

init(_Type, Req, _Opts) -> {ok, Req, []}.
-spec terminate(_,_,_) -> 'ok'.

terminate(_Reason, _Req, _State) -> ok.
