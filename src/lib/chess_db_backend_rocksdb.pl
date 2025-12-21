
:- use_module(library(rocksdb)).

chess_db_connect_handle( Dir, Handle ) :-
    rocks_open( Dir, Handle, [] ).

chess_db_create( Dir, Base, Dbh ) :-
     chess_db_rocksdb_table_fields( Base, Key, Val ),
     rocks_open( Dir, Dbh, [key(Key),value(Val)] ),
     ( Base == game_info -> rocks_put(Dbh, max_id, 0); true ).

chess_db_holds( game_posi(_Roxi), Db, Args, Val ) :-
     ( Args = [Key|_] -> true; Args = Key ),
     rocks_get( Db, Key, Val ).

chess_db_table_update( game_posi(_Roxi), Db, [Inpo,_Mv], Next ) :-
     rocks_put( Db, Inpo, Next ).

chess_db_game_info_exists( KVs, Dbh, ExGid ) :-
     findall( KVa, (member(K-V,KVs),atomic_list_concat([K,V],':',KVa)), KVas ),
     atomic_list_concat( KVas, ';', InfoAtm ),
     rocks_get( Dbh, InfoAtm, ExGid ).

chess_db_game_add_info( Dbh, Info, Gid ) :-
     findall( KVa, (member(K-V,KVs),atomic_list_concat([K,V],':',KVa)), KVas ),
     atomic_list_concat( KVas, ';', InfoAtm ),
     rocks_put( Dbh, InfoAtm, Gid ).

chess_db_max_id( HandleST, Max ) :-
    ( atomic(HandleST) -> Dbh = HandleST; chess_db_handle(info,HandleST,Dbh) ),
    rocks_get( Dbh, max_id, Max ).

chess_db_inc_id( Dbh, Gid ) :-
     rocks_put( Dbh, max_id, Gid ).

% chess_db_rocksdb_table_fields(game_info, int64, term).   % Gid -> InfosList -> [keyInfo-valInfo|...]
chess_db_rocksdb_table_fields(game_info, atom, int64).     % InfosList (=> atom(K:V;KVs)) -> Gid  // max_id -> 0
chess_db_rocksdb_table_fields(game_move, atom, term).      % Gid'+'ply -> [Hmv,Move]
chess_db_rocksdb_table_fields(game_orig, int64, atom).     % Gid -> original text Lines ? 
chess_db_rocksdb_table_fields(game_posi, int64, atom).
