
:- use_module(library(rocksdb)).

chess_db_holds( game_posi(_Roxi), Db, Args, Val ) :-
     ( Args = [Key|_] -> true; Args = Key ),
     rocks_get( Db, Key, Val ).

chess_db_table_update( game_posi(_Roxi), Db, [Inpo,_Mv], Next ) :-
     rocks_put( Db, Inpo, Next ).
