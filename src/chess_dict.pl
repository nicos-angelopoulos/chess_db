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
            hmv:0,  % half moves clock: since last 
            fmv:0,  % full moves: played so far
            8:10,16:8,24:9,32:11,40:12,48:9,56:8,64:10,
            7:7 ,15:7,23:7,31:7 ,39:7 ,47:7,55:7,63:7 ,
            6:0 ,14:0,22:0,30:0 ,38:0 ,46:0,54:0,62:0 ,
            5:0 ,13:0,21:0,29:0 ,37:0 ,45:0,53:0,61:0 ,
            4:0 ,12:0,20:0,28:0 ,36:0 ,44:0,52:0,60:0 ,
            3:0 ,11:0,19:0,27:0 ,35:0 ,43:0,51:0,59:0 ,
            2:1 ,10:1,18:1,26:1 ,34:1 ,42:1,50:1,58:1 ,
            1:4,  9:2,17:3,25:5 ,33:6 ,41:3,49:2,57:4,
            0:0 % white move = 0; black = 1
    }.

/** chess_dict.

A documentation predicate, 

Coding, each sqaure in board is in dictionary key: 1-64 (see algebraic_turn_piece/3).
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

Castling + Turn 
 * cwk:1
    can white still castle king side ?
 * cwq:1
    can white still castle queen side ?
 * cbk:1
    can black still castle king side ?
 * cbq:1
    can black still castle queen side ?
 * 0:0 
    active move turn (0:white to move, 1: black to move)

Clocks
 * hmv:0
     half moves clock: since last take
 * fmv:0
     full moves: played so far

*/
chess_dict.

/** chess_dict_inpo( ?Dict, ?Inpo ).

Convert between dictionary and (long) integer position representation.

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

So in db storing might need a position table (for the numeric only) + (half.move +full.move table) to make it tranlseatable to dics.

@author nicos angelopoulos
@version  0:1 19.9.15

*/
chess_dict_inpo( Dict, Inpo ) :-
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
    sumlist( [CTFact,ENFact|Factors], InpoNum ),
    atom_number( Inpo, InpoNum ).
% fixme: incomplete.
chess_dict_inpo( Dict, Inpo ) :-
    var( Dict ),
    DictE = board{},
    chess_dict_inpo( 1, DictE, Inpo, Dict ).

chess_dict_inpo( 67, DictE, _Inpo, Dict ) :-
    !,
    DictE = Dict.
chess_dict_inpo( I, DictE, Inpo, Dict ) :-
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

