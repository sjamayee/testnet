-module(testnet_hasher).
-export([doit/1, bin_to_hex/1]).

-spec doit(_) -> any().
doit(X) -> hash:doit(X, constants:hash_size()).
-spec bin_to_hex(binary()) -> [string()].
bin_to_hex(<<>>) -> "";
bin_to_hex(<<A, B/binary>>) ->
    byte_to_hex(<<A>>) ++ bin_to_hex(B).

-spec byte_to_hex(<<_:8>>) -> [string(),...].
byte_to_hex(<< N1:4, N2:4 >>) ->
    
    [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
