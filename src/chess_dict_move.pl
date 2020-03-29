:- lib(chess_algebraic_turn_piece/3).
:- lib(chess_pos_coord/3).

/** chess_dict_move( +Move, +DictI, +Turn, -DictO ).
    chess_dict_move( +Move, +DictI, -DictO ).

Enact Move and Turn in dictionary board DictI into new chess position DictO.

When Turn is given as a variable then it is instantiated to the move turn (DictI.0) in the dictionary), if it is non var/1, then it is 
checked against turn in DictI- throwing a ball if they do not match.

==
?- chess_dict_start_board(Start), chess_dict_move(e4,Start,0,Mid), chess_dict_move('Nc6',Mid,1,End).
Start = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps: (-), fmv:0, hmv:0},
Mid = board{0:1, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:0, 35:0, 36:1, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps: (-), fmv:0, hmv:0},
End = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:0, 35:0, 36:1, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps: (-), fmv:1, hmv:1}.


?- chess_dict_start_board(Start), chess_dict_move(e4,Start,One), chess_dict_move(h5,One,Two), chess_dict_move('Ke2',Two,Thr), chess_dict_move('Rh6',Thr,For).
Start = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps: (-), fmv:0, hmv:0},
One = board{0:1, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:0, 35:0, 36:1, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:35, fmv:0, hmv:0},
Two = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:0, 35:0, 36:1, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:7, 62:0, 63:0, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:62, fmv:1, hmv:0},
Thr = board{0:1, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:0, 34:6, 35:0, 36:1, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:7, 62:0, 63:0, 64:10, cbk:1, cbq:1, cwk:0, cwq:0, eps: (-), fmv:1, hmv:1},
For = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:0, 34:6, 35:0, 36:1, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:7, 62:10, 63:0, 64:0, cbk:1, cbq:0, cwk:0, cwq:0, eps: (-), fmv:2, hmv:2}.

?- chess_dict_start_board(Start), chess_dict_move(d4,Start,Turn1,One), chess_dict_move(e5,One,Turn2,Two).
Start = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:1, 27:0, 28:0, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps: (-), fmv:0, hmv:0},
Turn1 = 0,
One = board{0:1, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:0, 27:0, 28:1, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:0, 38:0, 39:7, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:27, fmv:0, hmv:0},
Turn2 = 1,
Two = board{0:0, 1:4, 2:1, 3:0, 4:0, 5:0, 6:0, 7:7, 8:10, 9:2, 10:1, 11:0, 12:0, 13:0, 14:0, 15:7, 16:8, 17:3, 18:1, 19:0, 20:0, 21:0, 22:0, 23:7, 24:9, 25:5, 26:0, 27:0, 28:1, 29:0, 30:0, 31:7, 32:11, 33:6, 34:1, 35:0, 36:0, 37:7, 38:0, 39:0, 40:12, 41:3, 42:1, 43:0, 44:0, 45:0, 46:0, 47:7, 48:9, 49:2, 50:1, 51:0, 52:0, 53:0, 54:0, 55:7, 56:8, 57:4, 58:1, 59:0, 60:0, 61:0, 62:0, 63:7, 64:10, cbk:1, cbq:1, cwk:1, cwq:1, eps:38, fmv:1, hmv:0}.
==

@author nicos angelopoulos
@version  0:1 2020/03/27

*/
chess_dict_move( Move, DictI, DictO ) :- 
    chess_dict_move( Move, DictI, DictI.0, DictO ).

chess_dict_move( Move, DictI, Turn, DictO ) :- 
    debug( chess(move), 'Move: ~w', [Move] ),
    Durn = DictI.0,
    ( var(Turn) -> 
        Turn = Durn
        ;
        ( Durn =:= Turn -> true; throw(turn_mismatch(Durn,Turn)) )
    ),
    !,
    put_dict( eps, DictI, '-', DictJ ),
    (chess_dict_move_1(Move,DictJ,Turn,DictM) -> true; throw(failed_on_move(Move,Turn,DictJ)) ),
    ( Turn =:= 1 -> 
        chess_dict_inc( DictM, fmv, DictL ),
        put_dict( 0, DictL, 0, DictO )
        ;
        put_dict( 0, DictM, 1, DictO )
    ).

chess_dict_move_1( MoveCheck, DictI, Turn, DictO ) :- 
    atom_concat( Move, '+', MoveCheck ),
    !,
    chess_dict_move_1( Move, DictI, Turn, DictO ).
chess_dict_move_1( 'O-O', DictI, Turn, DictO ) :- 
    !,
    chess_dict_move_castle_short( Turn, DictI, DictJ ),
    ( Turn =:= 0 ->
        % cwk:1,cwq:1,
        put_dict( cwk, DictJ, 0, DictK ),
        put_dict( cwq, DictK, 0, DictO )
        ;
        % cbk:1,cbq:1,
        put_dict( cbk, DictJ, 0, DictK ),
        put_dict( cbq, DictK, 0, DictO )
    ).
chess_dict_move_1( 'O-O-O', DictI, Turn, DictO ) :- 
    !,
    chess_dict_move_castle_long( Turn, DictI, DictJ ),
    ( Turn =:= 0 ->
        % cwk:1,cwq:1,
        put_dict( cwk, DictJ, 0, DictK ),
        put_dict( cwq, DictK, 0, DictO )
        ;
        % cbk:1,cbq:1,
        put_dict( cbk, DictJ, 0, DictK ),
        put_dict( cbq, DictK, 0, DictO )
    ).
% pawn.promotion
chess_dict_move_1( Move, DictI, Turn, DictO ) :- 
    atomic_list_concat( [Left,Right], '=', Move ),
    !,
    chess_algebraic_turn_piece( Right, Turn, NewPiece ),
    ( atomic_list_concat([FromPfx,To],x,Left) ->
        chess_algebraic_square( To, ToSqr ),
        (Turn =:= 0 -> FromSfx ='7' ; FromSfx = '2'),
        atom_concat( FromPfx, FromSfx, FromAlg ),
        chess_algebraic_square( FromAlg, FromSqr ),
        put_dict( FromSqr, DictI, 0, DictJ ),
        put_dict( ToSqr, DictJ, NewPiece, DictK ),
        put_dict( hmv, DictK, 0, DictO )
        ;
        chess_algebraic_square( Left, Sqr ),
        (Turn =:= 0 -> From is Sqr - 1 ; From is Sqr + 1),
        put_dict( From, DictI, 0, DictJ ),
        put_dict( Sqr, DictJ, NewPiece, DictK ),
        put_dict( hmv, DictK, 0, DictO )
    ).
% pawn.normal
chess_dict_move_1( Move, DictI, Turn, DictO ) :- 
    % polymorphic DictO -> if same as Dict1 use destructive assignment
    atom_codes( Move, [BegC|Cs] ),
    BegC > 96,
    !,
    chess_dict_move_pawn( BegC, Cs, DictI, Move, Turn, true, DictO ).
% piece
chess_dict_move_1( Move, DictI, Turn, DictO ) :- 
    atom_codes( Move, [PieceC,BegC|Cs] ),
    PieceC < 97,
    !,
    chess_dict_move_piece( PieceC, BegC, Cs, DictI, Move, Turn, true, DictO ).
chess_dict_move_1( Move, _DictI, _Turn, _DictO ) :- 
    throw( unimplemented_move(Move) ).

% short Castle white
chess_dict_move_castle_short( 0, DictI, DictO ) :-
    DictI.33 =:= 6,  % white king
    DictI.41 =:= 0,
    DictI.49 =:= 0,
    DictI.57 =:= 4,  % white rook
    chess_dict_move_piece_from_to( DictI, 33-6, 49-6, true, DictM ),
    chess_dict_move_piece_from_to( DictM, 57-4, 41-4, true, DictN ),
    chess_dict_flip_turn_to( DictN, 1, DictO ).

chess_dict_move_castle_short( 1, DictI, DictO ) :-
    DictI.40 =:= 12,  % black king
    DictI.48 =:= 0,
    DictI.56 =:= 0,
    DictI.64 =:= 10,  % black rook
    chess_dict_move_piece_from_to( DictI, 40-12, 56-12, true, DictM ),
    chess_dict_move_piece_from_to( DictM, 64-10, 48-10, true, DictN ),
    chess_dict_flip_turn_to( DictN, 0, DictO ).

% long Castle white
chess_dict_move_castle_long( 0, DictI, DictO ) :-
    DictI.33 =:= 6,  % white king
    DictI.25 =:= 0,
    DictI.17 =:= 0,
    DictI.9  =:= 0,
    DictI.1  =:= 4,  % white rook
    chess_dict_move_piece_from_to( DictI, 33-6, 17-6, true, DictM ),
    chess_dict_move_piece_from_to( DictM,  1-4, 25-4, true, DictN ),
    chess_dict_flip_turn_to( DictN, 1, DictO ).
chess_dict_move_castle_long( 1, DictI, DictO ) :-
    DictI.40 =:= 12,  % black king
    DictI.32 =:= 0,
    DictI.24 =:= 0,
    DictI.16 =:= 0,
    DictI.8  =:= 10,  % black rook
    chess_dict_move_piece_from_to( DictI, 40-12, 24-12, true, DictM ),
    chess_dict_move_piece_from_to( DictM,  8-10, 32-10, true, DictN ),
    chess_dict_flip_turn_to( DictN, 0, DictO ).

% piece
chess_dict_move_piece( PieceC, BegC, [NumC], DictI, _Move, Turn, Constr, DictO ) :-
    BegC > 96,
    0'0 =< NumC,
    NumC =< 0'9,
    !,
    chess_piece_code_turn( PieceC, Turn, Piece ),
    findall( PossPos, (between(1,64,PossPos), Piece =:= DictI.PossPos, call(Constr,PossPos)), PossPoss ),
    (  Piece > 6 -> ProtoPiece is Piece - 6; ProtoPiece is Piece ),
    chess_codes_pos( BegC, NumC, EndPos ),
    include( chess_dict_move_possible(ProtoPiece,DictI,EndPos), PossPoss, Starts ),
    ( Starts = [StartPos] ->
        chess_dict_move_piece_from_to(DictI, StartPos-Piece, EndPos-Piece, true, DictN),
        chess_dict_inc( DictN, hmv, DictM ),
        chess_dict_flip_turn_from( DictM, Turn, DictO )
        ;
        throw( non_unique_starts_1(Starts) )
    ).
% Nce4, N3e4
chess_dict_move_piece( PieceC, DscC, [BegC,NumC], DictI, _Move, Turn, Constr, DictO ) :-
    BegC > 96,
    0'0 =< NumC,
    NumC =< 0'9,
    !,
    chess_piece_code_turn( PieceC, Turn, Piece ),
    findall( PossPos, (between(1,64,PossPos), Piece =:= DictI.PossPos, call(Constr,PossPos)), PossPoss ),
    (  Piece > 6 -> ProtoPiece is Piece - 6; ProtoPiece is Piece ),
    chess_codes_pos( BegC, NumC, EndPos ),
    include( chess_dict_move_possible(ProtoPiece,DictI,EndPos), PossPoss, Starts ),
    ( Starts = [StartPos] ->
        chess_dict_move_piece_from_to( DictI, StartPos-Piece, EndPos-Piece, true, DictN ),
        chess_dict_inc( DictN, hmv, DictM ),
        chess_dict_flip_turn_from( DictM, Turn, DictO )
        ;
        ( chess_dict_positions_uniqued(Starts,DscC,StartPos) ->
            chess_dict_move_piece_from_to( DictI, StartPos-Piece, EndPos-Piece, true, DictN ),
            chess_dict_inc( DictN, hmv, DictM ),
            chess_dict_flip_turn_from( DictM, Turn, DictO )
            ;
            atom_codes( ToSqr, [BegC,NumC] ),
            atom_codes( PieceAtm, [PieceC] ),
            throw( non_unique_starts_2(Starts,ToSqr,PieceAtm,DictI) )
        )
    ).
% Ncxe4, N3xe4
chess_dict_move_piece( PieceC, DscC, [0'x,BegC,NumC], DictI, Move, Turn, Constr, DictO ) :-
    !,
    put_dict( hmv, DictI, 0, DictM ),
    chess_dict_move_piece( PieceC, DscC, [BegC,NumC], DictM, Move, Turn, Constr, DictO ).
% piece takes:
chess_dict_move_piece( PieceC, 0'x, [BegC,NumC], DictI, Move, Turn, Constr, DictO ) :-
    !,
    put_dict( hmv, DictI, 0, DictM ),
    chess_dict_move_piece( PieceC, BegC, [NumC], DictM, Move, Turn, Constr, DictO ).
chess_dict_move_piece( PieceC, FromC, [BegC,NumC], DictI, Move, Turn, _ConstrIn, DictO ) :-
    % fixme: check ConstrIn is = true
    BegC > 96,
    0'0 =< NumC,
    NumC =< 0'9,
    !,
    ( (0'0 =< FromC, FromC =< 0'9) ->
        number_codes( FromRow, [FromC] ),
        Constr = on_row(FromRow)
        ;
        FromC >= 0'a, FromC =< 0'h, 
        atom_codes( FromCol, [FromC] ),
        Constr = on_col(FromCol)
    ),
    chess_dict_move_piece( PieceC, BegC, [NumC], DictI, Move, Turn, Constr, DictO ).

chess_piece_code_turn( Code, Turn, Piece ) :-
    chess_piece_code_turn( Code, PiecePrv ),
    ( Turn =:= 0 -> Piece = PiecePrv
        ; Piece is PiecePrv + 6
    ).

chess_piece_code_turn( 0'N, 2 ).
chess_piece_code_turn( 0'B, 3 ).
chess_piece_code_turn( 0'R, 4 ).
chess_piece_code_turn( 0'Q, 5 ).
chess_piece_code_turn( 0'K, 6 ).

chess_dict_positions_uniqued( Starts, DscC, Unique ) :-
    ( DscC < 0'a -> Dsc = row; Dsc = col ),
    chess_dict_positions_uniqued( Dsc, DscC, Starts, Unique ).

chess_dict_positions_uniqued( col, DscC, Poss, Unique ) :-
    TrgClm is DscC - 0'a + 1,
    findall( Pos, (member(Pos,Poss),TrgClm is (((Pos - 1) // 8) + 1)), [Unique] ).

chess_dict_positions_uniqued( row, DscC, Poss, Unique ) :-
    TrgRow is DscC - 0'0,
    findall( Pos, (member(Pos,Poss),TrgRow is (Pos mod 8) ), [Unique] ).

% Knights
chess_dict_move_possible( 2, _Dict, ToPos, FromPos ) :-
    % member( Dist, [-17,-15,-10,-6,6,10,15,17] ),
    % FromPos is ToPos + Dist,
    chess_pos_coord( ToPos, X, Y ),
    member(Zx,[1,2,-1,-2]), 
    member(Zy,[1,2,-1,-2]), 
    abs(Zx) =\= abs(Zy),
    X1 is X + Zx, X1 > 0, X1 < 9,
    Y1 is Y + Zy, Y1 > 0, Y1 < 9,
    chess_pos_coord( FromPos, X1, Y1 ),
    !.  % fixme: this should be higher up surely ???
% Bishops
% 30 can be landed from 39, 48 (upper right);  23, 16 (upper left); 21, 12, 3 (lower left) 37, 44, 51, 58 (lower right)
%
chess_dict_move_possible( 3, _Dict, ToPos, FromPos ) :-
    member( Div, [7,9] ),
    0 =:= (abs(ToPos - FromPos) mod Div),
    Steps is abs( ((ToPos - 1) // 8) - ( (FromPos - 1) //8) ) - 1, % diff in columns
    Steps > -1, % -1 means they are no same column
    Steps is ( abs(ToPos - FromPos) // Div ) - 1,
    !.
% Rooks, same row
chess_dict_move_possible( 4, Dict, ToPos, FromPos ) :-
    0 =:= (abs(ToPos - FromPos) mod 8 ),
    Steps is (abs(ToPos - FromPos) // 8) - 1,
    Min is min(ToPos,FromPos),
    findall( Btw, (between(1,Steps,Step),Btw is Min + (8 * Step), Dict.Btw =:= 0), Btws ),
    length( Btws, Steps ),
    !.
% Rooks, same column
chess_dict_move_possible( 4, Dict, ToPos, FromPos ) :-
    Clm is (FromPos - 1) // 8,
    Clm is (ToPos - 1) // 8,
    Steps is abs(ToPos - FromPos) - 1,
    Min is min(ToPos,FromPos),
    findall( Btw, (between(1,Steps,Step),Btw is Min + Step, Dict.Btw =:= 0), Btws ),
    length( Btws, Steps ),
    !.
% Queens = bishop or rook
chess_dict_move_possible( 5, Dict, ToPos, FromPos ) :-
    chess_dict_move_possible( 3, Dict, ToPos, FromPos ).
chess_dict_move_possible( 5, Dict, ToPos, FromPos ) :-
    chess_dict_move_possible( 4, Dict, ToPos, FromPos ).
% fixme: King
chess_dict_move_possible( 6, _Dict, ToPos, FromPos ) :-
    member( Diff, [1,-1,8,-8,-7,-9,+7,+9] ),
    FromPos is ToPos + Diff,
    !.

% no need to do anything with DictI.eps 
% 
chess_dict_move_pawn( FromC, [0'x,BegC,NumC], DictI, _Move, Turn, _ConstrI, DictO ) :-
    0'a =< FromC, FromC =< 0'h,
    !,
    chess_codes_pos( BegC, NumC, EndPos ),
    % chess_piece_pawn_turn( Turn, Pawn ),
    chess_dict_move_pawn_takes( Turn, FromC, EndPos, DictI, DictN ),
    put_dict( hmv, DictN, 0, DictM ),
    chess_dict_flip_turn_from( DictM, Turn, DictO ).
% pawn push
chess_dict_move_pawn( BegC, [NumC], DictI, Move, Turn, Constr, DictO ) :-
    0'0 =< NumC,
    NumC =< 0'9,
    !,
    chess_codes_pos( BegC, NumC, EndPos ),
    chess_piece_pawn_turn( Turn, Pawn ),
    chess_move_piece_pawn_turn_step( Turn, EndPos, 1, Single ),
    ( (call(Constr,Single),DictI.Single =:= Pawn) -> 
        % ignore DictI.eps for single square moves
        chess_dict_move_piece_from_to(DictI, Single-Pawn, EndPos-Pawn, true, DictN),
        put_dict( hmv, DictN, 0, DictM ),
        chess_dict_flip_turn_from( DictM, Turn, DictO )
        ;
        chess_move_piece_pawn_turn_step( Turn, EndPos, 2, Double ),
        ( (call(Constr,Double),DictI.Double =:= Pawn) -> 
            chess_move_piece_pawn_turn_step( Turn, EndPos, 1, EnPassant ),
            chess_dict_move_piece_from_to(DictI, Double-Pawn, EndPos-Pawn, true, DictN),
            put_dict( hmv, DictN, 0, DictM ),
            chess_fen_square( EnPAlg, EnPassant ),
            % put_dict( eps, DictM, EnPassant, DictL ),
            put_dict( eps, DictM, EnPAlg, DictL ),
            chess_dict_flip_turn_from( DictL, Turn, DictO )
            ;
            throw( cannot_find_pawn_to_move_to(Move) )
        )
    ).

chess_dict_move_pawn_takes( 0, FromC, EndPos, DictI, DictO ) :-
    Pawn = 1,
    ColMax is (FromC - 0'a) * 8,
    ( EndPos > ColMax -> SrcPos is EndPos - 9; SrcPos is EndPos + 7 ),
    DictI.SrcPos =:= Pawn,
    ( DictI.EndPos =\= 0 -> 
        chess_dict_move_piece_from_to( DictI, SrcPos-Pawn, EndPos-Pawn, true, DictO )
        ; % throw(no_en_passe_yet(EndPos,SrcPos,0)) 
        RemPos is EndPos - 1,
        ( DictI.RemPos =\= 7 -> throw( messed_up_en_passant(DictI.RemPos,EndPos,SrcPos) )
            ;
            chess_dict_move_piece_from_to( DictI, SrcPos-Pawn, EndPos-Pawn, RemPos, DictO )
            % throw(no_en_passe_yet(EndPos,SrcPos,0)) 
        )
    ).
chess_dict_move_pawn_takes( 1, FromC, EndPos, DictI, DictO ) :-
    Pawn = 7,
    ColMax is (FromC - 0'a) * 8,
    ( EndPos > ColMax -> SrcPos is EndPos - 7; SrcPos is EndPos + 9),
    DictI.SrcPos =:= Pawn,
    ( DictI.EndPos =\= 0 -> true; throw(no_en_passe_yet(EndPos,SrcPos,1)) ),
    chess_dict_move_piece_from_to( DictI, SrcPos-Pawn, EndPos-Pawn, true, DictO ).

chess_dict_move_piece_from_to( DictI, PosFrom-PieceFrom, PosTo-PieceTo, Remove, DictO ) :-
    put_dict( PosFrom, DictI, 0, DictM ),
    put_dict( PosTo, DictM, PieceTo, DictN ),
    ( Remove == true -> 
        DictN = DictK
        ;
        put_dict( Remove, DictN, 0, DictK )
    ),
    chess_dict_move_de_castles( PieceFrom, PosFrom, DictK, DictO ).

chess_dict_move_de_castles( 6, 33, DictI, DictO ) :-
    % cwk:1,cwq:1,
    !,
    put_dict( cwk, DictI, 0, DictK ),
    put_dict( cwq, DictK, 0, DictO ).
chess_dict_move_de_castles( 11, 40, DictI, DictO ) :-
    % cbk:1,cbq:1,
    put_dict( cbk, DictI, 0, DictJ ),
    put_dict( cbq, DictJ, 0, DictO ).
chess_dict_move_de_castles( 4, 1, DictI, DictO ) :-
    !,
    put_dict( cwk, DictI, 0, DictO ).
chess_dict_move_de_castles( 4, 57, DictI, DictO ) :-
    !,
    put_dict( cwq, DictI, 0, DictO ).
chess_dict_move_de_castles( 10, 8, DictI, DictO ) :-
    !,
    put_dict( cbk, DictI, 0, DictO ).
chess_dict_move_de_castles( 10, 64, DictI, DictO ) :-
    !,
    put_dict( cbq, DictI, 0, DictO ).
chess_dict_move_de_castles( _, _, DictI, DictO ) :-
    !,
    DictI = DictO.

/*
chess_dict_move_de_castles( PieceFrom, PosFrom, DictK, DictO ) :-
    here(PieceFrom,PosFrom,DictK,DictO).
    */

chess_piece_pawn_turn( 0, 1 ).
chess_piece_pawn_turn( 1, 7 ).

chess_move_piece_pawn_turn_step( 0, Pos, Inc, Source ) :-
    Source is Pos - Inc.
chess_move_piece_pawn_turn_step( 1, Pos, Inc, Source ) :-
    Source is Pos + Inc.

chess_codes_pos( BegC, NumC, Pos ) :-
    number_codes( Num, [NumC] ),
    End8Pad is BegC - 0'a,
    Pos is (End8Pad * 8) + Num.

chess_dict_flip_turn_from( DictN, FromBit, DictO ) :-
    flip_turn_bit( FromBit, ToBit ),
    chess_dict_flip_turn_to( DictN, ToBit, DictO ).

flip_turn_bit(0, 1).
flip_turn_bit(1, 0).

chess_dict_flip_turn_to( DictN, ToBit, DictO ) :-
    chess_dict_flip_turn_bit( ToBit, DictN, DictO ),
    !.
chess_dict_flip_turn_to( _DictN, ToBit, _DictO ) :-
    throw( un_recognised_bit(ToBit) ).

chess_dict_flip_turn_bit( 1, DictN, DictO ) :-
    chess_dict_flip_bits( DictN.0, 1 ),
    put_dict( 0, DictN, 1, DictO ).
chess_dict_flip_turn_bit( 0, DictN, DictO ) :-
    chess_dict_flip_bits( DictN.0, 0 ),
    put_dict( 0, DictN, 0, DictO ).

chess_dict_flip_bits( Bit1, Bit2 ) :-
    1 is Bit1 + Bit2,
    !.
chess_dict_flip_bits( Bit1, Bit2 ) :-
    throw( incompatible_flipping_bits(Bit1,Bit2) ).

true(_X).
