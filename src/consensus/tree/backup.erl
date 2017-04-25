-module(backup).
-export([hash/0, backup/0, backup_files/0, read/2, read_size/1, files/0]).
-spec files() -> [any(),...].
files() -> [constants:blocks(), constants:block_pointers(), constants:accounts(), constants:all_secrets(), constants:d_accounts(), constants:channels(), constants:d_channels(), constants:entropy()].

-spec backup_files() -> [any()].
backup_files() -> tl(tl(files())).
-spec hash() -> any().
hash() -> hash(backup_files(), []).
-spec hash([any()],[any()]) -> any().
hash([], X) -> hash:doit(X);
hash([F|T], X) -> hash(T, [hash:file(F)|X]).
-define(backup, "backup/").
-spec backup() -> 'ok'.
backup() -> backup(backup_files()).
-spec backup([[atom() | [any()] | char()]]) -> 'ok'.
backup([]) -> ok;
backup([F|T]) -> 
    file:copy(F, ?backup++F),
    backup(T).

-define(word, constants:word_size()).
-spec read_size([atom() | [any()] | char()]) -> non_neg_integer().
read_size(File) ->
    filelib:file_size(?backup++File) div ?word.
-spec read(binary() | maybe_improper_list(atom() | binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | []),integer()) -> binary() | string().
read(File, N) ->
    {ok, RFile } = file:open(?backup++File, [read, binary, raw]),
    {ok, Out} = file:pread(RFile, N*?word, ?word),
    file:close(RFile),
    Out.

