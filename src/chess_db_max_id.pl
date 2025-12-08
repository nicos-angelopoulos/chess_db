/**  chess_db_max_id( +HandleST, -Max )

For a handle or term of handles (in which case the first argument is used),
return the Max id of the first column of the game_info on the Handle.

For an empty database, =|Max = 1|= is true.

==
?- chess_db_current( CdbHs ), chess_db_max_id( CdbHs, Max ).
Max = 31.
==

@author nicos angelopoulos
@version  0.1 2018/3/15

*/
chess_db_max_id( HandleST, Max ) :-
    ( atomic(HandleST) -> Handle = HandleST; chess_db_handle(info,HandleST,Handle) ),
    ( (db_max(Handle,game_info,1,Max),Max\=='',Max\=='$null$') -> true; Max is 1).
