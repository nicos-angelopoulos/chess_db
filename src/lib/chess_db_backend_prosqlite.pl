
chess_db_holds( Db, _Table, Args, Val ) :-
     ( Args = [Key|_] -> true; Args = Key ),
     rocks_get( Db, Key, Val ).

chess_db_table_update( game_posi(kvx), Db, [Inpo,_Mv], Next ) :-
     db_retractall( Db, game_posi(Inpo,_), _ ),
     db_assert( Db, game_posi(Inpo,Next), _ ).
