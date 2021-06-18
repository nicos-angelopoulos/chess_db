/** chess_db_position( +PosOrMoves, -DbHandles, -Conts ).

For each open chess_db connection DbHandles, and for a Pos(ition) or list of Moves, returns the Continuations in this chess database.

Please notice that this a better way to look for position than chess_db_opening/2 as it also find transpositions.

Following from the example in the module docs, pack(chess_db):

==
?- chess_db_connect( short, db(Short) ), assert(short_db_handle(Short)).
Short = '/home/nicos/pl/packs/private/chess_db/short'.
==

The following two queries are equivalent.

==
?- chess_db_position( [e4,e6], Handles, Conts ).
Handles = chdbs(<sqlite>(0xaaaae1482af0), <sqlite>(0xaaaae1487890), <sqlite>(0xaaaae148c740), <sqlite>(0xaaaae1491500)),
Conts = '1-3-d3;31-3-d4' ;
false

?-  chess_dict_start_board(Board0),chess_dict_move(e4,Board0,Board1),chess_dict_move(e6,Board1,Board2),chess_dict_inpo(Board2,Inpo),
    chess_db_position( Inpo, Handles, Conts ).
...
:2},
Inpo = ...,
Handles = chdbs(<sqlite>(0xaaaae1482af0), <sqlite>(0xaaaae1487890), <sqlite>(0xaaaae148c740), <sqlite>(0xaaaae1491500)),
Conts = '1-3-d3;31-3-d4' ;
false.
==

@author nicos angelopoulos
@version  0:1 2021/6/18

*/
chess_db_position( PoM, CdbHs, Conts ) :-
    ( is_list(PoM) -> 
        chess_dict_start_board( Soard ),
        chess_dict_moves( PoM, Soard, DictPos ),
        chess_dict_inpo( DictPos, Posi )
        ; 
        Posi = PoM
    ),
    chess_db_current( CdbHs ),
    chess_db_handle( posi, CdbHs, PosiH ),
    db_holds( PosiH, game_posi(Posi,Conts) ).

chess_dict_moves( [], Board, Board ).
chess_dict_moves( [Mv|Mvs], BoardI, BoardO ) :-
    chess_dict_move( Mv, BoardI, BoardM ),
    chess_dict_moves( Mvs, BoardM, BoardO ).
