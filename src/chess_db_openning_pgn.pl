/**  chess_db_openning_pgn( Moves, PgnF ).

Dump all the originial PGN fragments that correspond to Moves openning, to<br>
the output file PgnF.

==
?- chess_db( pack('chess_db/examples/4ncl_short.pgn'), [dir(short),create(true)] ).
?- chess_db_connect( [dir(short),profile(false)] ).
?- PgnF = 'short/french.pgn', chess_db_openning_pgn( [e4,e6], PgnF ).
==

@author nicos angelopoulos
@version  0.1 2018/3/17
@see pack('examples/short.pl')

*/
chess_db_openning_pgn( Moves, PgnF ) :-
    io_open( PgnF, write, PgnOut ),
    chess_db_openning_pgn_stream( Moves, PgnOut ),
    io_close( PgnF, PgnOut ).

chess_db_openning_pgn_stream( Moves, PgnOut ) :-
    chess_db_openning( Moves, CdbHs:Gid ),
    chess_db_handle( orig, CdbHs, OrigHa ),
    db_holds( OrigHa, game_orig(Gid,Orig) ),
    write( PgnOut, Orig ),
    fail.
chess_db_openning_pgn_stream( _Moves, _PgnOut ).
