:- lib(binary/2).

/** chess_dict_start_board( -Board ).

Dictionary of the start board position.

==
?- chess_dict_start_board( Dcb ).
Dcb = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps: 0, fmv:1, hmv:0}.
==

@author nicos angelopoulos
@version  0:1 2020/03/26

*/
chess_dict_start_board( Board ) :-
    Board = board{
            % castling
            cwk:1,cwq:1,cbk:1,cbq:1,
            % en passant square
            eps:0,
            hmv:0,  % half moves clock: since last take (0 if this is a take, 1 if last was, and this isn't)
            fmv:0,  % full moves: played so far
            8:10,16:8,24:9,32:11,40:12,48:9,56:8,64:10,
            7:7 ,15:7,23:7,31:7 ,39:7 ,47:7,55:7,63:7 ,
            6:0 ,14:0,22:0,30:0 ,38:0 ,46:0,54:0,62:0 ,
            5:0 ,13:0,21:0,29:0 ,37:0 ,45:0,53:0,61:0 ,
            4:0 ,12:0,20:0,28:0 ,36:0 ,44:0,52:0,60:0 ,
            3:0 ,11:0,19:0,27:0 ,35:0 ,43:0,51:0,59:0 ,
            2:1 ,10:1,18:1,26:1 ,34:1 ,42:1,50:1,58:1 ,
            1:4,  9:2,17:3,25:5 ,33:6 ,41:3,49:2,57:4 ,
            0:0 % white move = 0; black = 1
    }.

/** chess_dict.

A documentation predicate.

Coding each square in board is in dictionary key: 1-64 (see algebraic_turn_piece/3).
Numbering starts at bottom left and goes numbers (rows) first before it sweeps to the next column. 

Values for each square key are:
  *  0 empty
  *  1 white pawn 
  *  2 white knight
  *  3 white bishop 
  *  4 white rook
  *  5 white queen
  *  6 white king
  *  7 black pawn   (P)
  *  8 black knight
  *  9 black bishop 
  * 10 black rook
  * 11 black queen
  * 12 black king

En passant, eps
  * eps(Eps=0)
  position for En passant-able pawn
  [here we store the number, unlike FEN which stores the algebraic]

Booleans: Castling, Turn and last was a take
  * cwk:1
    can white still castle king side ?
  * cwq:1
    can white still castle queen side ?
  * cbk:1
    can black still castle king side ?
  * cbq:1
    can black still castle queen side ?
  % * lwt:0
    % last move was a take (1 if true)
  * 0:0 
    active move turn (0:white to move, 1: black to move)

Clocks
  * hmv:0
     half moves clock: since last take  (0 if this is a take, 1 if last was, and this isn't)
  * fmv:0
     full moves: played so far

This is how the empty Board dictinary is defined.
==
chess_dict_start_board( Board ) :-
    Board = board{
            cwk:1,cwq:1,cbk:1,cbq:1,
            eps:0,
            fmv:0,
            hmv:0,
            8:10,16:8,24:9,32:11,40:12,48:9,56:8,64:10,
            7:7 ,15:7,23:7,31:7 ,39:7 ,47:7,55:7,63:7 ,
            6:0 ,14:0,22:0,30:0 ,38:0 ,46:0,54:0,62:0 ,
            5:0 ,13:0,21:0,29:0 ,37:0 ,45:0,53:0,61:0 ,
            4:0 ,12:0,20:0,28:0 ,36:0 ,44:0,52:0,60:0 ,
            3:0 ,11:0,19:0,27:0 ,35:0 ,43:0,51:0,59:0 ,
            2:1 ,10:1,18:1,26:1 ,34:1 ,42:1,50:1,58:1 ,
            1:4,  9:2,17:3,25:5 ,33:6 ,41:3,49:2,57:4 ,
            0:0
    }.
==
*/
chess_dict.

/** chess_dict_inpo_obsolete( ?Dict, ?Inpo ).

Convert between dictionary and (long) integer position representation.

This is the OLD IMPLEMENTATION, please use chess_dict_inpo/2 instead.

Dict is a chess_dict/0 dictionary and Inpo is very long integer.

This is a bit incomplete as Dict has a bit more information than Inpo.
The extra information should be added in a structured extension of Inpo (say Int/Hmv+Fmv), or extra arg(s).

==
?- chess_dict_start_board( Bd ), chess_dict_inpo( Bd, Inpo ), chess_dict_inpo( New, Inpo ).

Bd = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:0, fmv:1, hmv:0},
Inpo = 100700000000010408070000000001020907000000000103120700000000010611070000000001050907000000000103080700000000010210070000000001040030,
New = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:0}.

?- chess_dict_start_board( Bd ), chess_dict_inpo( Bd, Inpo ),                                                                                                                        Lipo is 100 ^ 66.

Bd = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:0, fmv:1, hmv:0},
Inpo = 100700000000010408070000000001020907000000000103120700000000010611070000000001050907000000000103080700000000010210070000000001040030,
Lipo = 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.

==

We also need to factor in [castling + turn to move] + en passant square ;  BUT NOT (?) halfmove clock and fullmove number.

So in db storing might need a position table (for the numeric only) + (half.move +full.move table) to make it transletable to dics.

@author nicos angelopoulos
@version  0:1 19.9.15

*/
chess_dict_inpo_obsolete( Dict, Inpo ) :-
    ground( Dict ),
    !,
    findall( Factor, ( between(1,64,I), Rel is I + 1, 
                       Factor is (100 ^ Rel) * Dict.I, 
                       debug(chess_db(inpo),'Rel: ~d, Factor: ~w',[Rel,Factor])
                     ),
                            Factors 
           ),
    chess_fen_square( Dict.eps, ENFactNat ), ENFact is ENFactNat * 100,
    CTFact is Dict.0 + (Dict.cwk * 2) + (Dict.cwq * 4) + (Dict.cbk * 8) + (Dict.cbq * 16),
    sum_list( [CTFact,ENFact|Factors], InpoNum ),
    atom_number( Inpo, InpoNum ).
% fixme: incomplete.
chess_dict_inpo_obsolete( Dict, Inpo ) :-
    var( Dict ),
    DictE = board{},
    chess_dict_inpo( 1, DictE, Inpo, Dict ).

chess_dict_inpo_obsolete( 67, DictE, _Inpo, Dict ) :-
    !,
    DictE = Dict.
chess_dict_inpo_obsolete( I, DictE, Inpo, Dict ) :-
    V is Inpo mod (100 ^ I) // (100 ^ (I - 1)), 
    debug( chess_db(inpo), 'Component: ~w', [I+V] ),
    ( I =:= 1 -> 
        % CTFact is Dict.0 + (Dict.cwk * 2) + (Dict.cwq * 4) + (Dict.cbk * 8) + (Dict.cbq * 16),
        binary( V, BinAtm ),
        atom_codes( BinAtm, BinCs ),
        findall( Pad, (between(1,5,N),(nth1(N,BinCs,Pad)->true;Pad=0'0)), PadCs ),
        maplist( digit_code, [Bq,Bk,Wq,Wk,Mv], PadCs ),
            % cwk:1,cwq:1,cbk:1,cbq:1, 0:0 % white move = 0; black = 1
        put_dict( cwk, DictE, Wk, DictM1 ),
        put_dict( cwq, DictM1, Wq, DictM2 ),
        put_dict( cbk, DictM2, Bk, DictM3 ),
        put_dict( cbq, DictM3, Bq, DictM4 ),
        put_dict( 0, DictM4, Mv, DictM )
        ; 
        ( I =:= 2 -> 
            chess_fen_square( Val, V ),
            put_dict( eps, DictE, Val, DictM )
            ;
            Rel is I - 2,
            put_dict( Rel, DictE, V, DictM )
        )
    ),
    J is I + 1,
    chess_dict_inpo( J, DictM, Inpo, Dict ).
    
digit_code( Digi, Code ) :-
    number_codes( Digi, [Code] ).

chess_dict_inc( DictI, Key, DictO ) :-
    Val is DictI.Key + 1,
    put_dict( Key, DictI, Val, DictO ).

/** chess_dict_inpo( +Dict, -Inpo ).

We changing Inpo representation to something similar to training representation for stockfish which 
was also adopted by lichess in 2024 according to a blog on their web-site.

Encode the occupied squares + 4 bit for 16 piece encoding 
  * 0 an en-passant-able pawn; (we don't need empty square as we mark pieces)
  * 1-12 pieces; as in dictionary  (A,B,C)
  * 13 w rook avail for castle     (D)
  * 14 b rook avail for castle     (E)
  * 15 black king when is black to  move;  (F)

+ 8 bits for half move clock + 8 bits for ply + variant data.

==
?- chess_dict_start_board(Start), chess_dict_inpo(Start,Inpo).
Inpo = 4800136621178049995892176369597901265598272573836252205342.
==

This is less than half (58) the digits of the size of the old representation (132).

Also in atom represented binary 
==
40 ?- chess_dict_start_board(Start), chess_db:chess_dict_inpo_binary(Start,Bnpo).
Bnpo = '110000111100001111000011110000111100001111000011110000111100001111010001000111100010000100011000001100010001100101010001000110110110000100010110001100010001100100100001000110001101000100011110'.

==

@author nicos angelopoulos
@version  0:2 25.12.07,   new integer/binary representation
@version  0:3 26.1.28,    changed the binary to hex digits- and implemented the 
@see https://lichess.org/@/revoof/blog/adapting-nnue-pytorchs-binary-position-format-for-lichess/cpeeAMeY

*/

chess_dict_inpo( Dict, Inpo ) :-
     chess_inpo_occupancy_pieces( 0, Dict, Occ, Pcx ),
     append( Occ, Pcx, Hex ),
     atom_codes( Inpo, Hex ).

chess_inpo_occupancy_pieces( 64, _Dict, [], [] ) :- !.
chess_inpo_occupancy_pieces( I, Dict, [Hex1,Hex2|Texs], Pcx ) :-
     chess_inpo_occupancy_column_half( 1, 0, I, Dict, 0, Hex1, Pcx, PcxA ),
     chess_inpo_occupancy_column_half( 1, 4, I, Dict, 0, Hex2, PcxA, PcxB ),
     J is I + 8,
     chess_inpo_occupancy_pieces( J, Dict, Texs, PcxB ).

chess_inpo_occupancy_column_half( 5, _A, _I, _Dict, Dec, Hex, Pcx, Pct ) :- 
     !,
     chess_dec_hex( Dec, Hex ),
     Pcx = Pct.
chess_inpo_occupancy_column_half( J, A, I, Dict, Dec, Hex, Pcx, Pct ) :-
     K is I + J + A,
     Dk = Dict.K,
     ( Dk =:= 0 ->
          Dec1 is Dec,
          Pcx = Tcx
          ;
          Dec1 is Dec + ( 2 ^ (4-J) ),
          chess_dict_inpo_v2_piece( Dk, Dict, K, Px ),
          % chess_dict_inpo_v2_piece( Dk, Dict, K, PcInt ),
          % chess_dec_hex( PcInt, Px ),
          Pcx = [Px|Tcx]
     ),
     L is J + 1,
     chess_inpo_occupancy_column_half( L, A, I, Dict, Dec1, Hex, Tcx, Pct ).

chess_dec_hex(0, 0'0).    % en passant pawn
chess_dec_hex(1, 0'1).    % white pawn
chess_dec_hex(2, 0'2).    % white knight
chess_dec_hex(3, 0'3).    % white bishop
chess_dec_hex(4, 0'4).    % white rook
chess_dec_hex(5, 0'5).    % white queen
chess_dec_hex(6, 0'6).    % white king
chess_dec_hex(7, 0'7).    % black pawn
chess_dec_hex(8, 0'8).    % black knight 
chess_dec_hex(9, 0'9).    % black bishop
chess_dec_hex(10, 0'A).   % black rook
chess_dec_hex(11, 0'B).   % black queen
chess_dec_hex(12, 0'C).   % black king
chess_dec_hex(13, 0'D).   % white castling rook
chess_dec_hex(14, 0'E).   % black castling rook
chess_dec_hex(15, 0'F).   % black moving king 

chess_dict_inpo_wrong( Dict, Inpo ) :-
     chess_dict_inpo_binary( Dict, Bin ),
     binary( Inpo, Bin ).

/* Add this to public. */
chess_dict_inpo_binary_wrong( Dict, Bin ) :-
     ground( Dict ),
     !,
     findall( Bool, (between(0,7,I),between(1,8,J),K is (I * 8) + J, (Dict.K > 0 -> Bool is 1 ; Bool is 0)), Bools ),
     atomic_list_concat( Bools, Atom ),
     findall( Pc, ( between(0,7,I),between(1,8,J),K is (I * 8) + J, Dc = Dict.K, Dc > 0,
                    chess_dict_inpo_v2_piece(Dc,Dict,K,PcInt),int_padded_bin(PcInt,4,Pc)
                  ), Pcs ),
     atomic_list_concat( Pcs, Ptom ),
     atom_concat( Atom, Ptom, Bin ).

/** chess_dict_inpo_v2_piece(+Dc, +Dict, +K, -Pc).

Encode a piece within the Dict.

  *  1 white pawn 
  *  2 white knight
  *  3 white bishop 
  *  4 white rook
  *  5 white queen
  *  6 white king
  *  7 black pawn   (P)
  *  8 black knight
  *  9 black bishop 
  * 10 black rook
  * 11 black queen
  * 12 black king

  * eps(Eps=0)
Booleans: Castling, Turn and last was a take
  * cwk:1
    can white still castle king side ?
  * cwq:1
    can white still castle queen side ?
  * cbk:1
    can black still castle king side ?
  * cbq:1
  */
chess_dict_inpo_v2_piece( 1, Dict, K, Pc ) :-   % white pawn
     ( Dict.eps =:= K -> Pc is 0'0; Pc is 0'1 ).
chess_dict_inpo_v2_piece( 2, _Dict, _K, Pc ) :-   % white knight
     Pc is 0'2.
chess_dict_inpo_v2_piece( 3, _Dict, _K, Pc ) :-   % white bishop
     Pc is 0'3.
chess_dict_inpo_v2_piece( 4, Dict, K, Pc ) :-   % white rook
     ( K =:= 1 ->
          ( Dict.cwq =:= 1 ->
               % Pc is 13
               Pc is 0'D
               ;
               Pc is 0'4
          )
          ;
          % fixed: 26.01.28
          ( (K =:= 57, Dict.cwk =:= 1) ->
                    % Pc is 13
                    Pc is 0'D
                    ;
                    Pc is 0'4
          )
     ).
chess_dict_inpo_v2_piece( 5, _Dict, _K, Pc ) :-   % white queen
     Pc is 0'5.
chess_dict_inpo_v2_piece( 6, _Dict, _K, Pc ) :-   % white king
     Pc is 0'6.
chess_dict_inpo_v2_piece( 7, Dict, K, Pc ) :-   % black pawn
     ( Dict.eps =:= K -> Pc is 0'0; Pc is 0'7 ).
chess_dict_inpo_v2_piece( 8, _Dict, _K, Pc ) :-   % black knight
     Pc is 0'8.
chess_dict_inpo_v2_piece( 9, _Dict, _K, Pc ) :-   % black bishop
     Pc is 0'9.
chess_dict_inpo_v2_piece(10, Dict, K, Pc ) :-   % black rook
     ( K =:= 8 ->
          ( Dict.cbq =:= 1 ->
               % Pc is 14
               Pc is 0'E
               ;
               % Pc is 10
               Pc is 0'A
          )
          ;
          % fixed: 26.01.28
          ( (K =:= 64, Dict.cbk =:= 1) ->
                    % Pc is 14
                    Pc is 0'E
                    ;
                    % Pc is 10
                    Pc is 0'A
          )
     ).
chess_dict_inpo_v2_piece( 11, _Dict, _K, Pc ) :-  % black queen
     % Pc is 11.
     Pc is 0'B.
chess_dict_inpo_v2_piece( 12, Dict, _K, Pc ) :-   % black king, 15 indicates black to move- else white move is assumed
     % (Dict.0 =:= 1 -> Pc is 15 ; Pc is 6). <- bug 26.01.28
     (Dict.0 =:= 1 -> Pc is 0'F ; Pc is 0'C).

int_padded_bin( Int, N, Bin ) :-
     binary( Int, ShortBin ), 
     atom_codes( ShortBin, ShortCs ),
     length( ShortCs, Len ),
     ( Len < N -> 
          Nxt is Len + 1,
          findall( 0'0, between(Nxt,N,_), PadCs ),
          append( PadCs, ShortCs, Codes ),
          atom_codes( Bin, Codes )
          ;
          ( Len =:= N -> 
               atom_codes( Bin, ShortCs )
               ;
               throw( too_long_int(Int,int_padded_bin/3) )
          )
     ).
