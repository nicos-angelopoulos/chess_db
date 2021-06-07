:- lib(chess_dict_pos_coord/3).

/** chess_dict_pos_algebraic( +Pos, -Alg ).
    chess_dict_pos_algebraic( -Pos, +Alg ).


==
?- chess_dict_pos_algebraic(1, Alg).

?- between( 1, 64, I ), chess_dict_pos_algebraic( I, Alg ), write( I:Alg ), nl, fail.
1:a1
2:a2
3:a3
....
?- chess_dict_pos_algebraic(E4, e4).
==

*/
chess_dict_pos_algebraic( Pos, Alg ) :-
     ground( Pos ),
     !,
     chess_dict_pos_coord( Pos, X, Y ),
     L is 0'a + X - 1,
     M is 0'0 + Y,
     atom_codes( Alg, [L,M] ).
chess_dict_pos_algebraic( Pos, Alg ) :-
     ground( Alg ),
     atom_codes( Alg, [L,M] ),
     X is L - 0'a + 1,
     Y is M - 0'0,
     chess_dict_pos_coord( Pos, X, Y ).
