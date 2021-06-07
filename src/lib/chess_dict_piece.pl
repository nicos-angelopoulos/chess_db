/** chess_dict_piece(+DictPiece, +Colour, +Piece).

Table for converting between Dictionary piece code, its name and the colour of the piece.

==
?- chess_dict_piece( 1, Clr, Piece).
Clr = white,
Piece = pawn.

?- chess_dict_piece( DPiece, Clr, pawn ).
DPiece = 1,
Clr = white ;
DPiece = 7,
Clr = black.
==
*/
chess_dict_piece( 1, white, pawn ).
chess_dict_piece( 2, white, knight ).
chess_dict_piece( 3, white, bishop ).
chess_dict_piece( 4, white, rook ).
chess_dict_piece( 5, white, queen ).
chess_dict_piece( 6, white, king ).
chess_dict_piece( 7, black, pawn).
chess_dict_piece( 8, black, knight).
chess_dict_piece( 9, black, bishop).
chess_dict_piece(10, black, rook).
chess_dict_piece(11, black, queen).
chess_dict_piece(12, black, king).
