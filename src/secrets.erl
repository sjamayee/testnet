-module(secrets).
-export([new/0]).
-spec new() -> {_,binary()}.
new() ->
    Secret = crypto:strong_rand_bytes(12),
    Commit = hash:doit(Secret),
    {Commit, Secret}.
