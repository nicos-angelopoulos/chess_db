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
