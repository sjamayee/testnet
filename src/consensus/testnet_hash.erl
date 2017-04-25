-module(testnet_hash).
-export([doit/1,test/0,file/1,hash/1]).

-spec hash(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | [])) -> binary().
hash(S) -> 
    crypto:hmac(sha256, S, "").
-spec doit(_) -> binary().
doit(S) when is_binary(S) -> hash(S);
doit(S) -> hash(term_to_binary(S)).
-spec file(atom() | binary() | maybe_improper_list(atom() | binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | [])) -> atom() | binary().
file(S) -> 
    {ok, F} = file:open(S, [read, binary]),
    O = file(F, <<>>, 0),
    ok = file:close(F),
    O.
-spec file(pid() | {'file_descriptor',atom(),_},binary(),non_neg_integer()) -> atom() | binary().
file(F, O, S) ->
    B = 10000000,%need at least 10 megabytes of ram open
    case file:pread(F, B*S, B) of
	eof -> O;
	{ok, Out} -> file(F, hash(<<Out/binary, O/binary>>), S+1);
        {error, Reason} -> Reason
    end.
-record(p, {p = ""}).
-spec test() -> 'success'.
test() -> 
    %file("src/consensus/hash.erl"),
    file(constants:keys()),
    doit(123),
    doit(abc),
    doit([123]),
    doit([[[]]]),
    doit(#p{}),
    success.
