/** chess_dict_pos_coord( +Pos, -X, -Y ).
    chess_dict_pos_coord( -Pos, +X, +Y ).

Convert between Board position and coordinates.

Called from calculating Knight sources.

==
?- chess_dict_pos_coord( 26, X, Y ), chess_dict_pos_coord( Tsx, X, Y ).
X = 4,
Y = 2,
Tsx = 26.


% This is the d2 square.
==

*/

chess_dict_pos_coord( Pos, X, Y ) :-
    ground( Pos ),
    !,
    X is ((Pos - 1) // 8) + 1,
    Y is ((Pos - 1) mod 8) + 1.
chess_dict_pos_coord( Pos, X, Y ) :-
    Pos is (X-1) * 8 + Y.

/** chess_dict_pos_coord_codes( +Pos, -X, -Y ).
    chess_dict_pos_coord_codes( -Pos, +X, +Y ).

Convert between Board position and coordinates in terms of algebraic codes.

==
?- 
   chess_db:chess_dict_pos_coord_codes(26, X, Y), chess_db:chess_dict_pos_coord_codes(Tsx, X, Y),
   atom_codes( Xa, [X] ), atom_codes( Ya, [Y] ).

X = 100,
Y = 50,
Tsx = 26,
Xa = d,
Ya = '2'.

% This is the d2 square.
==

@tbd add to interface?

*/
chess_dict_pos_coord_codes( Pos, X, Y ) :-
     ground( Pos ),
     !,
     chess_dict_pos_coord( Pos, Xco, Yco ),
     X is Xco + 0'a - 1,
     Y is Yco + 0'0.
chess_dict_pos_coord_codes( Pos, X, Y ) :-
     Xco is X - 0'a + 1,
     Yco is Y - 0'0,
     chess_dict_pos_coord( Pos, Xco, Yco ).
