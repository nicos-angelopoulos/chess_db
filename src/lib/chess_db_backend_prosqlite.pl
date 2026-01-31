

chess_db_connect_handle( Db, _Base, Handle ) :-
    sqlite_connect( Db, Handle, as_predicates(false) ).

chess_db_game_info_exists( [], _InfHa, _ExGid ).
chess_db_game_info_exists( [K-V|T], InfHa, ExGid ) :-
     db_holds( InfHa, game_info(ExGid,K,V) ),
     chess_db_game_info_exists( T, InfHa, ExGid ).

chess_db_create( Db, Base, Handle ) :-
     sqlite_connect( Db, Handle, exists(false) ),
     chess_db_table_fields( Base, Cnms ),
     Goal =.. [Base|Cnms],
     db_create( Handle, Goal ).

chess_db_holds( game_posi(kvx), Db, Args, Val ) :-
     ( Args = [Key|_] -> true; Args = Key ),
     db_holds( Db, game_posi(Key,Val) ).

chess_db_table_update_orig( Dbh, Gid, Otm ) :-
     db_assert( Dbh, game_orig(Gid,Otm), _ ).

chess_db_table_update_posi( Db, Inpo, Next ) :-
     db_retractall( Db, game_posi(Inpo,_), _ ),
     db_assert( Db, game_posi(Inpo,Next), _ ).

chess_db_game_add_info( InfoHandle, Info, Gid ) :-
     findall( game_info(Gid,K,V), member(K-V,Info), Goals ),
     db_assert( InfoHandle, Goals, _ ).

chess_db_inc_id( _Dbh, _Gid ). % fixme: we should store this somewhere ?

chess_db_max_id( HandleST, Max ) :-
    ( atomic(HandleST) -> Handle = HandleST; chess_db_handle(info,HandleST,Handle) ),
    ( (db_max(Handle,game_info,1,Max),Max\=='',Max\=='$null$') -> true; Max is 0).

chess_db_limos_game_moves( Limos, Dbh, Nid ) :-
     findall( game_move(Nid,Ply,Hmv,NxtMv), (member(limo(Ply,Hmv,NxtMv,_Inpo),Limos),NxtMv \== []), Moals ),
     db_assert( Dbh, Moals, _ ).

chess_db_base_ext( Base, SqliteF ) :-
     file_name_extension( Base, sqlite, SqliteF ).

chess_db_table_fields( game_info, [gid+integer,key+text,value-text] ).
% chess_db_table_fields( game_move, [gid+integer,move_no+integer,turn+boolean,move-text] ).
chess_db_table_fields( game_move, [gid+integer,ply+integer,hmv-integer,move-text] ).
chess_db_table_fields( game_orig, [gid+integer,original-text] ).
% chess_db_table_fields( game_posi, [gid+integer,ply+integer,position-text] ).
% chess_db_table_fields( game_posi, [position+text,pairs-text] ).
chess_db_table_fields( game_posi, [position+integer,pairs-text] ).
