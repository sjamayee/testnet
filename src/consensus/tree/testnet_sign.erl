-module(testnet_sign).
-export([test/0,test2/1,test3/0,sign_tx/5,sign/2,verify_sig/3,shared_secret/2,verify/2,data/1,revealed/1,empty/1,empty/0,set_revealed/2,verify_1/2,verify_2/2, pubkey2address/1, valid_address/1, hard_new_key/0,new_key/0,pub/1,pub2/1,address2binary/1,binary2address/1]).
-record(signed, {data="", sig="", pub = "", sig2="", pub2="", revealed=[]}).
-define(cs, 8). %checksum size
-spec pub(#signed{}) -> any().
pub(X) -> X#signed.pub.
-spec pub2(#signed{}) -> any().
pub2(X) -> X#signed.pub2.
-spec empty() -> #signed{data::[],sig::[],pub::[],sig2::[],pub2::[],revealed::[]}.
empty() -> #signed{}.
-spec empty(_) -> #signed{sig::[],pub::[],sig2::[],pub2::[],revealed::[]}.
empty(X) -> #signed{data=X}.
-spec data(#signed{}) -> any().
data(X) -> X#signed.data.
-spec set_revealed(#signed{},_) -> #signed{}.
set_revealed(X, R) -> #signed{data = X#signed.data, sig = X#signed.sig, pub = X#signed.pub, sig2 = X#signed.sig2, pub2 = X#signed.pub2, revealed = R}.
-spec revealed(#signed{}) -> any().
revealed(X) -> X#signed.revealed.
-spec en(binary() | [1..255]) -> binary().
en(X) -> base64:encode(X).
-spec de(binary() | [1..255]) -> binary().
de(X) -> base64:decode(X).
-spec params() -> {{'prime_field',<<_:112,_:_*16>>} | {'characteristic_two_field',1..1114111,{'tpbasis',9 | 15 | 36 | 62 | 68 | 69 | 74 | 87 | 120 | 158} | {'ppbasis',1 | 2 | 3 | 5,2 | 3 | 5 | 6 | 7,7 | 8 | 10 | 11 | 12 | 43 | 56 | 83 | 85}},{<<_:8,_:_*8>>,<<_:8,_:_*8>>,'none' | <<_:160>>},<<_:136,_:_*16>>,<<_:64,_:_*8>>,<<_:8,_:_*8>>}.
params() -> crypto:ec_curve(secp256k1).
-spec shared_secret(binary() | [1..255],binary() | [1..255]) -> binary().
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
%to_bytes(X) -> term_to_binary(X).
-spec to_bytes(_) -> any().
to_bytes(X) -> packer:pack(X).
-spec generate() -> any().
generate() -> crypto:generate_key(ecdh, params()).
-spec new_key() -> {binary(),binary()}.
new_key() -> %We keep this around for the encryption library. it is used to generate 1-time encryption keys.
    {Pub, Priv} = generate(),%crypto:generate_key(ecdh, params()),
    {en(Pub), en(Priv)}.
-spec sign(_,binary() | [1..255]) -> binary().
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, to_bytes(S), [de(Priv), params()])).
-spec verify_sig(_,binary() | [1..255],binary() | [1..255]) -> any().
verify_sig(S, Sig, Pub) -> 
    SD = de(Sig),
    PD = de(Pub),
    crypto:verify(ecdsa, sha256, to_bytes(S), SD, [PD, params()]).
-spec verify_1(#signed{sig::binary() | [1..255],pub::binary() | [1..255]},_) -> boolean().
verify_1(Tx, Addr) -> 
    Pub = Tx#signed.pub,
    B = verify_sig(Tx#signed.data, Tx#signed.sig, Pub),
    (Addr == pubkey2address(Pub)) and B.
-spec verify_2(#signed{sig2::binary() | [1..255],pub2::binary() | [1..255]},_) -> boolean().
verify_2(Tx, Addr) -> 
    Pub2 = Tx#signed.pub2,
    B = verify_sig(Tx#signed.data, Tx#signed.sig2, Pub2),
    (Addr == pubkey2address(Pub2)) and B.
-spec verify_both(#signed{sig::binary() | [1..255],pub::binary() | [1..255],sig2::binary() | [1..255],pub2::binary() | [1..255]},_,_) -> boolean().
verify_both(Tx, Addr1, Addr2) ->
    X = verify_1(Tx, Addr1),
    Y = verify_2(Tx, Addr1),
    if
        X -> verify_2(Tx, Addr2);
        Y -> verify_1(Tx, Addr2);
        true -> false
    end.
-spec type_check(_) -> boolean().
type_check(Type) -> %these are the types that get signed twice
    lists:any(fun(X) -> X==Type end, [gc, nc, ctc, spk]).
	%(Type == gc) or (Type == nc) or (Type == ctc) or (type == spk)

-spec verify(#signed{data::tuple(),sig::binary() | [1..255],pub::binary() | [1..255]},_) -> boolean().
verify(SignedTx, Accounts) ->
    Tx = SignedTx#signed.data,
    N1 = element(2, Tx),
    {_, Acc1, _Proof} = account:get(N1, Accounts),
    Type = element(1, Tx),
    B = type_check(Type),
    if
	B ->
	    N2 = element(3, Tx),
	    {_, Acc2, _Proof2} = account:get(N2, Accounts),
	    verify_both(SignedTx, account:addr(Acc1), account:addr(Acc2));
	true -> 
	    verify_1(SignedTx, account:addr(Acc1))
    end.
-spec sign_tx(tuple(),_,_,_,_) -> {'error',<<_:88>>} | #signed{data::tuple()}.
sign_tx(SignedTx, Pub, Priv, ID, Accounts) when element(1, SignedTx) == signed ->
    Tx = SignedTx#signed.data,
    R = SignedTx#signed.revealed,
    N = element(2, Tx),
    {_, Acc, _Proof} = account:get(N, Accounts),
    AAddr = account:addr(Acc),
    Addr = pubkey2address(Pub),
    if
	(AAddr == Addr) and (N == ID) -> 
	    Addr = account:addr(Acc),
	    Sig = sign(Tx, Priv),
	    #signed{data=Tx, sig=Sig, pub=Pub, sig2=SignedTx#signed.sig2, pub2=SignedTx#signed.pub2, revealed=R};
	true ->
	    %make sure Tx is one of the types that has 2 signatures: tc, ctc, gc
	    Type = element(1, Tx),
	    true = type_check(Type),
	    N2 = element(3, Tx),
	    {_, Acc2, _Proof2} = account:get(N2, Accounts),
	    BAddr = account:addr(Acc2),
	    if
		((Addr == BAddr) and (N2 == ID)) ->
		    Sig = sign(Tx, Priv),
		    #signed{data=Tx, sig=SignedTx#signed.sig, pub=SignedTx#signed.pub, sig2=Sig, pub2=Pub, revealed=R};
		true -> {error, <<"cannot sign">>}
	    end
	 end;
sign_tx(Tx, Pub, Priv, ID, Accounts) ->
    Sig = sign(Tx, Priv),
    N = element(2, Tx),
    {_, Acc, _Proof} = account:get(N, Accounts),
    AAddr = account:addr(Acc),
    Addr = pubkey2address(Pub),
    N2 = element(3, Tx),
    ST = if
	((Addr == AAddr) and (N == ID)) -> 
	    #signed{data=Tx, sig=Sig, pub=Pub};
	(N2 == ID) ->
	    {_, Acc2, _Proof2} = account:get(N2, Accounts),
	    Addr = account:addr(Acc2),
	    #signed{data=Tx, sig2=Sig, pub2=Pub};
	true -> {error, <<"cannot sign">>}
    end,
    ST.

-spec checksum(<<_:96>>) -> <<_:8>>.
checksum(X) -> checksum(0, X).
-spec checksum(non_neg_integer(),bitstring()) -> <<_:8>>.
checksum(N, <<H:4, T/bitstring>>) ->
    checksum(N+H, <<T/bitstring>>);
checksum(N, <<>>) ->
    M = N rem 256,
    <<M:8>>.

%looks like 60,000,000 keys per second costs about $1 a month. https://en.bitcoin.it/wiki/Vanitygen
%there are 2,600,000 seconds per month, so they can test 1.5*10^14 addresses for $1.
%assume the attacker has $1 billion, they can test 1.5*10^23 addresses.
%That is 77 bits.
%Assuming there are 1 million accounts with enough money in them that we can profitably attack simultaniously, add 20 bits
% 97 bits.
% make address generation 100,000 times more difficult, so we lose 15 bits.
% 82 bits.
% to stay ahead of moorse law speedup, add another two bits.
% 84 bits.

%there are 30 bits of humans (a little under 10 billion).
%by birthday problem, we need at least 60 bit of addresses to stop humans from randomly finding a collision.
%Each human is willing to pay up to $0.001 to buy an address.
%Attacker is willing to pay $1 billion to attack.
%there is 36 bits difference between
%If there were 1000000 people rich enough to be simultaniously attacked, then we need 20 more bits of security.
%36+20=56
%So at the minimum, an address would need to have 60 bits.
-define(AddressEntropy, constants:address_entropy()).
-spec pubkey2address(_) -> binary().
pubkey2address(P) when size(P) > 66 ->
    pubkey2address(base64:decode(P));
pubkey2address(P) ->
    %AB = (?AddressEntropy + 4),
    %BC = (hash:hash_depth()*8) - AB,
    %<< A:AB, T:BC >> = hash:doit(P),
    %S = T rem 5000,
    %case S of
	%0 ->
	    binary2address(testnet_hasher:doit(P)).%;
	%_ ->
	%    {error, invalid_pubkey}
    %end.
-spec address2binary(binary()) -> <<_:96>>.
address2binary(A) ->
    S = ?AddressEntropy,
    <<C:?cs, B:S>> = base58:base58_to_binary(binary_to_list((A))),
    <<C:?cs>> = checksum(<<B:S>>),
    <<B:S>>.
-spec binary2address(<<_:96>>) -> binary().
binary2address(B) -> 
    S = ?AddressEntropy,
    <<A:S>> = B,
    <<C:?cs>> = checksum(B),
    D = <<C:?cs, A:S>>,
    list_to_binary(base58:binary_to_base58(D)).
-spec valid_address(binary()) -> boolean().
valid_address(A) ->
    AB = ?AddressEntropy,
    << C:?cs, B:AB >> = base58:base58_to_binary(binary_to_list(A)),
    D = checksum(<<B:AB>>),
    << C:?cs >> == D.

-spec test() -> 'success'.
test() ->
    %{Address, Pub, Priv} = hard_new_key(), %recomputing is too slow. better to write it down, and reuse it each time.
    {Address, Pub, Priv} = hard_new_key(),
    {Address2, Pub2, Priv2} = hard_new_key(),
    %X = <<1,2,3,4>>,
    X = {abc, 3,6,3},
    Sig = sign(X, Priv),
    %io:fwrite(Sig),
    true = verify_sig(X, Sig, Pub),
    ID1 = 1,
    ID2 = 2,
    Acc = account:new(ID1, Address, 0, 0),
    Acc2 = account:new(ID2, Address2, 0, 0),
    Binary = address2binary(Address),
    Address = binary2address(Binary),
    Accounts1 = account:write(0, Acc),
    Accounts = account:write(Accounts1, Acc2),
    Tx = {ctc, ID1, ID2},
    Signed1 = sign_tx(Tx, Pub, Priv, ID1, Accounts), 
    Signed = sign_tx(Signed1, Pub2, Priv2, ID2, Accounts),
    Signed2 = sign_tx({spend, ID1, 0, ID2, 1, 1}, Pub, Priv, ID1, Accounts),
    Verbose = false,
    if
	Verbose ->
	    io:fwrite("pubkeys\n"),
	    io:fwrite(Pub),
	    io:fwrite("\n"),
	    io:fwrite(Pub2),
	    io:fwrite("\n"),
	    io:fwrite("privkeys\n"),
	    io:fwrite(Priv),
	    io:fwrite("\n"),
	    io:fwrite(Priv2),
	    io:fwrite("\n"),
	    io:fwrite("signed tx\n"),
	    io:fwrite(packer:pack(Signed)),
	    io:fwrite("\n");
	true -> ok
    end,
    true = verify(Signed2, Accounts),
    true = verify(Signed, Accounts),
    true = verify_both(Signed, Address, Address2),
    true = verify_both(Signed, Address2, Address),
    false = verify_both(Signed, Address, Address),
    true = valid_address(Address),
    success.
-spec hard_new_key() -> {binary(),binary(),binary()}.
hard_new_key() ->
    {Pub, Priv} = new_key(),
    Address = pubkey2address(Pub),
    {Address, Pub, Priv}.
-spec times(non_neg_integer(),fun(() -> any())) -> 'ok'.
times(0, _) -> ok;
times(N, F) ->
    F(),
    times(N-1, F).
-spec test2(non_neg_integer()) -> 'ok'.
test2(X) ->
    times(X, fun() -> generate() end ).
-spec test3() -> {integer(),_}.
test3() ->
    timer:tc(sign, hard_new_key, []).
		     

