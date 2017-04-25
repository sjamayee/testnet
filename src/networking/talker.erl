-module(talker).
-export([talk/3, local_talk/1]).

-spec peer(binary() | tuple(),integer()) -> [any(),...].

peer(IP, Port) ->
    %{ok, Address} = inet_parse:address(IP),
    L = size(IP),
    T = inet_parse:ntoa(IP),
    Z = case L of    
	    4 -> T;
	    8 -> "[" ++ T ++ "]"
	end,
    "http://" ++ Z ++ ":" ++ integer_to_list(Port) ++ "/".

-spec local_talk(_) -> any().

local_talk(Msg) ->
    Peer = "http://127.0.0.1:3011/",
    talk(Msg, Peer).
%talk(Msg) ->
%    Peer = "http://127.0.0.1:3010/",
%    talk(Msg, Peer).
-spec talk(_,[any(),...]) -> any().

talk(Msg, Peer) ->
    talk_helper(Msg, Peer, 5).
-spec talk_helper(_,[any(),...],0 | 1 | 2 | 3 | 4 | 5) -> any().

talk_helper(_, _, 0) -> {error, failed_connect};
talk_helper(Msg, Peer, N) ->
    PM = packer:pack(Msg),
    case httpc:request(post, {Peer, [], "application/octet-stream", iolist_to_binary(PM)}, [{timeout, 1000}], []) of
	{ok, {_Status, _Headers, []}} -> 
	    %io:fwrite("talk error 1"),
	    talk_helper(Msg, Peer, N-1);
	    %io:fwrite({Status, Headers}),
	    %{error, undefined};
	{error, socket_closed_remotely} ->
	    %io:fwrite("socket closed remotely \n"),
	    talk_helper(Msg, Peer, N-1);
	{ok, {_, _, R}} -> 
	    packer:unpack(R);
	{error, timeout} ->
	    %io:fwrite("talk error timeout"),
	    talk_helper(Msg, Peer, N-1);
	    %{error, timeout};
	{error, {failed_connect, _}} ->
	    %io:fwrite("talk error failed connect"),
	    talk_helper(Msg, Peer, N-1)
		
    end.
-spec talk(_,binary() | tuple(),integer()) -> any().

talk(Msg, IP, Port) -> talk(Msg, peer(IP, Port)).
