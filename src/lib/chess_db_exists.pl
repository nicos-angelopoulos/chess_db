/**  chess_db_exists( +DbLoc ).

Holds iff DbLoc is the absolute file path location of chess db.

==
?- 
    absolute_file_name(),
    chess_db_exists( 
==

@author nicos angelopoulos
@version  0.1 2018/8/16

*/
chess_db_exists( Loc ) :-
    exists_directory( Loc ).
