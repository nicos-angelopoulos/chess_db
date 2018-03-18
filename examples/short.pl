
:- lib(chess_db).
:- lib(prosqlite).

:- debug(chess_db).

short :-
    debug( chess_db(original) ),
    chess_db( pack('chess_db/data/4ncl_short.pgn'), [dir('short'),create(true)] ),
    chess_db_connect( [dir('short'),profile(false)] ),
    findall( Gid, (chess_db_openning([e4,e6],Gid),write(Gid),nl), Gids ),
    write( gids(Gids) ), nl.

french :-
    chess_db_connect( [dir('short'),profile(false)] ),
    chess_db_openning( [e4,e6], Gid ),
    write( gid(Gid) ), nl,
    fail.
french :-
    PgnF = 'short/french.pgn',
    write( writting_to_file(PgnF) ), nl,
    chess_db_openning_pgn( [e4,e6], PgnF ).

sqlite_test :-
    sqlite_connect( 'short/game_move.sqlite', MovHa ),
    sqlite_connect( 'short/game_orig.sqlite', OriHa ),
    write( handles(MovHa,OriHa) ), nl.
