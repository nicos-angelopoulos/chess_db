
:- use_module(library(bdb)).

chess_db_connect_handle( Dir, Base, Dbh ) :-
    chess_db_bdb_table_fields( Base, KeyT, ValT ),
    ( Base == game_info ->
          os_path( Par, _, Dir ),
          os_path( Par, game_info_rev, RvrStem ),
          os_ext( bdb, RvrStem, Rvr ),
          bdb_open( Dir, update, Dbi, [key(KeyT),value(ValT)] ),
          bdb_open( Rvr, update, Dbv, [key(ValT),value(KeyT)] ),
          Dbh = Dbi/Dbv
          ;
          bdb_open( Dir, update, Dbh, [key(KeyT),value(ValT)] )
     ).

chess_db_create( Dir, Base, Db ) :-
     chess_db_bdb_table_fields( Base, Key, Val ),
     bdb_open( Dir, update, Dbh, [key(Key),value(Val)] ),
     ( Base == game_info -> 
               bdb_put(Dbh, -1, '0'),
               os_path( Par, _, Dir ),
               os_path( Par, game_info_rev, Rvr ),
               bdb_open( Rvr, update, Dbv, [key(Val),value(Key)] ),
               Db = Dbh/Dbv
               ;
               Db = Dbh
     ).

chess_db_holds( Db, Query ) :-
     bdb_db_holds( Query, Db ).

bdb_db_holds( game_info(Gid), Db ) :-
     !,
     ( Db = Dbh/_Dbv -> true; Dbh = Db ),
     ( var(Gid) ->
          bdb_enum( Dbh, Gid, _ )
          ;
          bdb_get( Dbh, Gid, _ )
     ).
bdb_db_holds( Query, Db ) :-
     ( (\+var(Db),Db=Dbh/_) ->     % fixme: need to eleborate here. for now pick first handle if 2 exist (symmetrical tables)
               true
               ;
               Db = Dbh
     ),
     functor( Query, _, Arity ),
     ( Arity =:= 2 -> 
          arg( 1, Query, Fst ),
          arg( 2, Query, Sec ),
          ( ground(Fst) -> 
               bdb_get( Dbh, Fst, Sec )
               ;
               bdb_enum( Dbh, Fst, Sec )
          )
          ;
          throw( bdb_gen_query_arity_3(not_yet_implemented) )
     ).

/* 
chess_db_holds( game_posi(_Roxi), Db, Args, Val ) :-
     ( Args = [Key|_] -> true; Args = Key ),
     % ( Args = [KeyNum|_] -> true; Args = KeyNum ),
     % atom_number( Key, KeyNum ),
     bdb_get( Db, Key, Val ).
*/

chess_db_table_update_orig( Dbh, Gid, Otm ) :-
     bdb_put( Dbh, Gid, Otm ).

chess_db_table_update_posi( Db, Inpo, Next ) :-
     bdb_put( Db, Inpo, Next ).

chess_db_game_info_exists( KVs, _Dbh/Dbv, ExGid ) :-
     findall( KVa, (member(K-V,KVs),atomic_list_concat([K,V],':',KVa)), KVas ),
     atomic_list_concat( KVas, ';', InfoAtm ),
     bdb_get( Dbv, InfoAtm, ExGid ).

chess_db_limos_game_moves( Dbh, Nid, Limos ) :-
     findall( NxtMv-Hmv, (member(limo(_Ply,Hmv,NxtMv,_Inpo),Limos), NxtMv \== []), Mvs ),
     % atomic_list_concat( Mvs, ';', MvsAtm ),
     bdb_put( Dbh, Nid, Mvs ).

chess_db_game_add_info( Dbh/Dbv, Info, Gid ) :-
     findall( KVa, (member(K-V,Info),atomic_list_concat([K,V],':',KVa)), KVas ),
     atomic_list_concat( KVas, ';', InfoAtm ),
     bdb_put( Dbh, Gid, InfoAtm ),
     bdb_put( Dbv, InfoAtm, Gid ).

chess_db_max_id( HandleST, Max ) :-
     ( atomic(HandleST) -> Dbh = HandleST; 
          ( HandleST = Dbh/_ -> true
                              ; chess_db_handle(info,HandleST,Dbh) 
          )
     ),
     bdb_get( Dbh, -1, MaxAtm ),
     atom_number( MaxAtm, Max ).

chess_db_inc_id( Dbh/_, Gid ) :-
     atom_number( GidAtom, Gid ),
     bdb_put( Dbh, -1, GidAtom ).

chess_db_base_ext( Base, DbF ) :-
     file_name_extension( Base, bdb, DbF ).

chess_db_bdb_table_fields(game_info, c_long, atom).   % Gid -> InfosList -> [keyInfo-valInfo|...]
chess_db_bdb_table_fields(game_move, c_long, term).      % Gid -> list(Mv)
chess_db_bdb_table_fields(game_orig, c_long, atom).     % Gid -> original text Lines ? 
chess_db_bdb_table_fields(game_posi, atom, atom).
