
chess_db_create( Db, Base, Handle ) :-
     sqlite_connect( Db, Handle, exists(false) ),
     chess_db_table_fields( Base, Cnms ),
     Goal =.. [Base|Cnms],
     db_create( Handle, Goal ).

chess_db_connect_handle( Db, Handle ) :-
    sqlite_connect( Db, Handle, as_predicates(false) ).

chess_db_holds( game_posi(kvx), Db, Args, Val ) :-
     ( Args = [Key|_] -> true; Args = Key ),
     db_holds( Db, game_posi(Key,Val) ).

chess_db_table_update( game_posi(kvx), Db, [Inpo,_Mv], Next ) :-
     db_retractall( Db, game_posi(Inpo,_), _ ),
     db_assert( Db, game_posi(Inpo,Next), _ ).

chess_db_table_fields( game_info, [gid+integer,key+text,value-text] ).
% chess_db_table_fields( game_move, [gid+integer,move_no+integer,turn+boolean,move-text] ).
chess_db_table_fields( game_move, [gid+integer,ply+integer,hmv-integer,move-text] ).
chess_db_table_fields( game_orig, [gid+integer,original-text] ).
% chess_db_table_fields( game_posi, [gid+integer,ply+integer,position-text] ).
% chess_db_table_fields( game_posi, [position+text,pairs-text] ).
chess_db_table_fields( game_posi, [position+integer,pairs-text] ).
