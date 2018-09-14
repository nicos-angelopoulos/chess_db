/**  chess_db_ids_pgn( +GidS, +PgnF ).

Save a number of games corresponding to list of, or single, id structures from the current dbs to a pgn file (PgnF).

==
?- chess_db_ids_pgn( ..., ... ).
==

@author nicos angelopoulos
@version  0.1 2018/8/19

*/
chess_db_ids_pgn( GidS, PgnF ) :-
    en_list( GidS, Gids ),
    PgnFOpts = [access(write),extensions([pgn,'PGN',''])],
    absolute_file_name( PgnF, AbsPgnF, PgnFOpts ),
    io_open( AbsPgnF, write, PgnOut ),
    chess_db_ids_pgn_stream( Gids, PgnOut ),
    io_close( AbsPgnF, PgnOut ).

chess_db_ids_pgn_stream( [], _PgnOut ).
chess_db_ids_pgn_stream( [CdbHs:Gid|T], PgnOut ) :-
    debug( chess_db(gid), 'Retrieving original for: ~w', CdbHs:Gid ),
    chess_db_handle( orig, CdbHs, OrigHa ),
    once( db_holds( OrigHa, game_orig(Gid,Orig) ) ),
    write( PgnOut, Orig ),
    chess_db_ids_pgn_stream( T, PgnOut ).
