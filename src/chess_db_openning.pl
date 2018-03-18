
:- lib(chess_db_move_next/4).

/** chess_db_openning( +Moves, -Gid ).
    chess_db_openning( +Moves, -Gid, +Opts ).

Gid is an identifier for a game starting with sequence of Moves.<br>
Moves is a list of atoms in standard chess notation.

GidStr is of the form GdbHs:Gid.

Games are taken from data based identified by chess_db_current/2.

Opts are passed to chess_db_current/2.

==
?- chess_db_openning( [e4,e6], Gid ). % find a French defense game.
==

@author nicos angelopoulos
@version  0.1 2018/3/15

*/
chess_db_openning( Moves, Gid ) :-
    chess_db_openning( Moves, Gid, [] ).

chess_db_openning( Moves, CdbHs:Gid, Args ) :-
    chess_db_current( CdbHs, Args ),
    chess_db_handle( move, CdbHs, MovHle ),
    chess_db_openning_moves( Moves, 1, false, MovHle, Gid ).

chess_db_openning_moves( [], _NoMov, _Dir, _MovHle, Gid ) :-
    \+ var(Gid).
chess_db_openning_moves( [Mv|Mvs], NoMov, Dir, MovHle, Gid ) :-
    db_holds( MovHle, game_move(Gid,NoMov,Dir,Mv) ),
    chess_db_move_next( Dir, NoMov, NxDir, NxNoMov ),
    chess_db_openning_moves( Mvs, NxNoMov, NxDir, MovHle, Gid ).
