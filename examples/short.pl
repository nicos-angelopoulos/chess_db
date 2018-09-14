
:- lib(chess_db).
:- lib(prosqlite).

:- debug(short).
:- debug(chess_db).

short :-
    debug( chess_db(original) ),
    absolute_file_name( short, Here ),
    ( chess_db_connect(Here)  ->
        debug( short, '(short) Using existing chess db: ~p', Here )
        ; 
        debug( short, '(short) Using existing chess db: ~p', Here ),
        chess_db( pack('chess_db/data/pgn/4ncl_short.pgn'), Here, create(true) )
    ),
    chess_db_disconnect,
    % chess_db_connect( short, profile(false) ),  
    french_db( FrenchDb ),
    french_disp,
    debug( short, '(short) Disconnecting chess db: ~p', Here ),
    chess_db_disconnect( FrenchDb ).

french :-
    french_db( FrenchDb ),
    french_disp,
    french_write,
    chess_db_disconnect( FrenchDb ).

french_db( French ) :-
    chess_db_connect( short, [db(French),profile(false)] ).

french_disp :-
    debug( short, '(short) Following games start with 1.e4,e6', true ),
    chess_db_opening( [e4,e6], Gid ),
    write( gid(Gid) ), nl,
    fail.
french_disp.

french_write :-
    PgnF = 'short/french.pgn',
    debug( short, '(short) Writing 1.e4,e6 starting games to: ~p', PgnF ),
    chess_db_opening_pgn( [e4,e6], PgnF ).

sqlite_test :-
    sqlite_connect( 'short/game_move.sqlite', MovHa ),
    sqlite_connect( 'short/game_orig.sqlite', OriHa ),
    write( handles(MovHa,OriHa) ), nl,
    sqlite_disconnect( MovHa ),
    sqlite_disconnect( OriHa ).
