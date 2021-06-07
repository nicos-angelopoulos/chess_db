/** chess_fen_square( ?Fen, ?Square ).

As chess_dict_pos_algebraic/2 but allows null location which is used in FEN for en passat-able square.

==
?- chess_fen_square( '-', Sq ).
Sq = 0.

?- chess_fen_square( A1, 1 ).
A1 = a1.

==

@author nicos angelopoulos
@version  0:1 2020/3/26

*/
chess_fen_square( '-', 0 ) :- !.
chess_fen_square( Alg, Sqr ) :-
    chess_dict_pos_algebraic( Sqr, Alg ).
