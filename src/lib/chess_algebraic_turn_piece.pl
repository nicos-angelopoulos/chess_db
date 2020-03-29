/** chess_algebraic_turn_piece( +Alge, +Turn, -PieceCode ).

Map between PGN written form and piece code in the context of Turn.

Turn is 0 for white and (where King, say is code 6) and Turn is 1 for black - where King's code is 12.

Succeeds exactly once.

==
?- chess_algebraic_turn_piece( 'K', 0, Code ).
Code = 6.

?- chess_algebraic_turn_piece( 'K', 1, Code ).
Code = 12.
==

@author nicos angelopoulos
@version  1:0 2020/3/28

*/
chess_algebraic_turn_piece( Alge, Turn, Code ) :-
    chess_algebraic_turn_piece_1( Alge, Turn, Code ),
    !.

chess_algebraic_turn_piece_1('', 0, 1 ).
chess_algebraic_turn_piece_1('P', 0, 1 ).
chess_algebraic_turn_piece_1('N', 0, 2 ).
chess_algebraic_turn_piece_1('B', 0, 3 ).
chess_algebraic_turn_piece_1('R', 0, 4 ).
chess_algebraic_turn_piece_1('Q', 0, 5 ).
chess_algebraic_turn_piece_1('K', 0, 6 ).

chess_algebraic_turn_piece_1('', 1, 7).
chess_algebraic_turn_piece_1('P', 1, 7).
chess_algebraic_turn_piece_1('N', 1, 8).
chess_algebraic_turn_piece_1('B', 1, 9).
chess_algebraic_turn_piece_1('R', 1, 10).
chess_algebraic_turn_piece_1('Q', 1, 11).
chess_algebraic_turn_piece_1('K', 1, 12).
