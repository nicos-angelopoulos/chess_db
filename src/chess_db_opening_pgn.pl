/**  chess_db_opening_pgn( +Moves, +PgnF ).

Dump all the original PGN fragments that correspond to Moves opening, to file PgnF.

==
?- chess_db( pack('chess_db/examples/4ncl_short.pgn'), short, [create(true)] ).
?- chess_db_connect( short, profile(false) ).
?- PgnF = 'short/french.pgn', chess_db_opening_pgn( [e4,e6], PgnF ).
==

@author nicos angelopoulos
@version  0.1 2018/3/17
@see pack('examples/short.pl')

*/
chess_db_opening_pgn( Moves, PgnF ) :-
    io_open( PgnF, write, PgnOut ),
    chess_db_opening_pgn_stream( Moves, PgnOut ),
    io_close( PgnF, PgnOut ).

chess_db_opening_pgn_stream( Moves, PgnOut ) :-
    chess_db_opening( Moves, Gid ), 
    Gid = CdbHs:Gno,
    debug( chess_db(gid), 'opening game: ~w', Gid ),
    chess_db_handle( orig, CdbHs, OrigHa ),
    db_holds( OrigHa, game_orig(Gno,Orig) ),
    write( PgnOut, Orig ),
    fail.
chess_db_opening_pgn_stream( _Moves, _PgnOut ).
