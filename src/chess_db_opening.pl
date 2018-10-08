
:- lib(chess_db_move_next/4).

/** chess_db_opening( +Moves, ?GameS ).

GameS is either a variable or a PGN filename.
The predicate identifies all games starting with sequence of Moves.
It either returns all game ids one at a time or saves them all the original PGN fragments
for all matching ids to the output file.
Moves is a list of atoms in standard chess notation.<br>
GidStr is of the form GdbHs:Gid.
Games are taken from data based identified by chess_db_current/1.

==
?- chess_db_opening( [e4,e6], Gid ). % find a French defence game.
==

@author nicos angelopoulos
@version  0.1 2018/3/15

*/
chess_db_opening( Moves, Var ) :-
    var( Var ),
    !,
    chess_db_opening_game_id( Moves, Var ).
chess_db_opening( [], PgnF ) :-
    % this a shorthand/speacial case
    !,
    findall( Gid, chess_db_game(Gid), Gids ),
    chess_db_ids_pgn( Gids, PgnF ).
chess_db_opening( Moves, PgnF ) :-
    chess_db_opening_pgn( Moves, PgnF ).

chess_db_opening_game_id( Moves, CdbHs:Gid ) :-
    chess_db_current( CdbHs ),
    chess_db_handle( move, CdbHs, MovHle ),
    chess_db_opening_moves( Moves, 1, false, MovHle, Gid ).

chess_db_opening_moves( [], _NoMov, _Dir, _MovHle, Gid ) :-
    \+ var(Gid).
chess_db_opening_moves( [Mv|Mvs], NoMov, Dir, MovHle, Gid ) :-
    db_holds( MovHle, game_move(Gid,NoMov,Dir,Mv) ),
    chess_db_move_next( Dir, NoMov, NxDir, NxNoMov ),
    chess_db_opening_moves( Mvs, NxNoMov, NxDir, MovHle, Gid ).
