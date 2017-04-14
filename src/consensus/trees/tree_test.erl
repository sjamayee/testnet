-module(tree_test).
-export([test/0]).

test() ->
    S = success,
    S = accounts:test(),
    S = channels:test(),
    S = existence:test(),
    S = active_oracles:test(),
    S.
    
