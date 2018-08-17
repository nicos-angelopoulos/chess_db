/**  chess_db_exists( +DbLoc ).

Holds iff DbLoc is the absolute file path location of chess db.<br>
Currently only works for directories which are assumed to hold prosqlite db/tables.<br>
Currently, not in the module interface.

==
?- 
    absolute_file_name(pack('chess_db/examples/short'), Abs ),
    chess_db:chess_db_exists( Abs ).

Abs = '.../swipl-7.7.18/pack/chess_db/examples/short'.
==

@author nicos angelopoulos
@version  0.1 2018/8/16

*/
chess_db_exists( Loc ) :-
    exists_directory( Loc ).
