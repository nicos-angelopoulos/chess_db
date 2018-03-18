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
chess_db_move_next( false, NoMov, true, NoMov ).
chess_db_move_next( true, NoMov, false, NxMov ) :-
    NxMov is NoMov + 1.
