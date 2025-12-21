
:- use_module(library(rocksdb)).

chess_db_connect_handle( Dir, Handle ) :-
    rocks_open( Dir, Handle, [] ).

chess_db_create( Dir, Base, Db ) :-
     chess_db_rocksdb_table_fields( Base, Key, Val ),
     rocks_open( Dir, Dbh, [key(Key),value(Val)] ),
     ( Base == game_info -> 
               rocks_put(Dbh, -1, 0),
               os_path( Par, _, Dir ),
               os_path( Par, game_info_rev, Rvr ),
               rocks_open( Rvr, Dbv, [key(Val),value(Key)] ),
               Db = Dbh/Dbv
               ;
               Db = Dbh
     ).

chess_db_holds( game_posi(_Roxi), Db, Args, Val ) :-
     ( Args = [KeyNum|_] -> true; Args = KeyNum ),
     atom_number( Key, KeyNum ),
     rocks_get( Db, Key, Val ).

chess_db_table_update( game_posi(_Roxi), Db, [InpoNum,_Mv], Next ) :-
     atom_number( Inpo, InpoNum ),
     rocks_put( Db, Inpo, Next ).

chess_db_game_info_exists( KVs, _Dbh/Dbv, ExGid ) :-
     findall( KVa, (member(K-V,KVs),atomic_list_concat([K,V],':',KVa)), KVas ),
     atomic_list_concat( KVas, ';', InfoAtm ),
     rocks_get( Dbv, InfoAtm, ExGid ).

chess_db_limos_game_moves( Dbh, Nid, Limos ) :-
     findall( NxtMv-Hmv, member(limo(_Ply,Hmv,NxtMv,_Inpo),Limos), Mvs ),
     % atomic_list_concat( Mvs, ';', MvsAtm ),
     rocks_put( Dbh, Nid, Mvs ).

chess_db_game_add_info( Dbh/Dbv, Info, Gid ) :-
     findall( KVa, (member(K-V,Info),atomic_list_concat([K,V],':',KVa)), KVas ),
     atomic_list_concat( KVas, ';', InfoAtm ),
     rocks_put( Dbh, Gid, InfoAtm ),
     rocks_put( Dbv, InfoAtm, Gid ).

chess_db_max_id( HandleST, Max ) :-
     ( atomic(HandleST) -> Dbh = HandleST; 
          ( HandleST = Dbh/_ -> true
                              ; chess_db_handle(info,HandleST,Dbh) 
          )
     ),
     rocks_get( Dbh, -1, Max ).

chess_db_inc_id( Dbh/_, Gid ) :-
     rocks_put( Dbh, -1, Gid ).


chess_db_base_ext( Base, DbF ) :-
     file_name_extension( Base, rocksdb, DbF ).

chess_db_rocksdb_table_fields(game_info, int64, term).   % Gid -> InfosList -> [keyInfo-valInfo|...]
% chess_db_rocksdb_table_fields(game_info, atom, int64).     % InfosList (=> atom(K:V;KVs)) -> Gid  // -1 => max_int -> 0
% chess_db_rocksdb_table_fields(game_move, atom, term).      % Gid'+'ply -> [Hmv,Move]
chess_db_rocksdb_table_fields(game_move, int64, term).      % Gid -> list(Mv)
chess_db_rocksdb_table_fields(game_orig, int64, atom).     % Gid -> original text Lines ? 
chess_db_rocksdb_table_fields(game_posi, atom, atom).
