/**  chess_db_move_next( +Dir, +NoMov, -NxDir, -NxNoMov )

Description

Opts
  * opt(Opt=_)
     is a...

==
==

@author nicos angelopoulos
@version  0.1 2018/3/15

*/
chess_db_move_next( 0, NoMov, 1, NoMov ).
chess_db_move_next( 1, NoMov, 0, NxMov ) :-
    NxMov is NoMov + 1.
