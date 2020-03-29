
chess_annotate_freq_defaults( postfix(annot) ).

/** chess_annotate_freq( +PgnF, +CheDb, +Opts ).

Annotate the games in PGN with frequency statistics from CheDb.

Opts
  * postfix(Psf=annot)
  postfix for the new file

@author nicos angelopoulos
@version  0:1 2020/03/29
@tbd allow multiple Dbs
@tbd break freqs as per result (reserve one for unknowns) ? 

*/
chess_annotate_freq( Pgn, CheDbLoc, Args ) :-
    Self = chess_annotate_freq,
    options_append( Self, Args, Opts ),
    % pgn( Pgn, PgnTerms, abs(Abs) ),
    pgn( Pgn, PgnTerms ),
    chess_db_connect( CheDbLoc, [handles(CheDb),db(AbsLoc)] ),
    write( abs_db_loc(AbsLoc) ), nl,
    maplist( chess_annotate_pgn_term(CheDb), PgnTerms, AnnTerms ),
    options( postfix(Psf), Opts ),
    % os_postfix( Psf, Abs, OutF ),
    os_postfix( Psf, Pgn, OutF ),
    pgn( OutF, AnnTerms ).

chess_annotate_pgn_term( CheDb, pgn(Info,Moves,Res,Orig), pgn(Info,Neves,Res,Orig) ) :-
    chess_dict_start_board( Start ),
    chess_annotate_pgn_moves( Moves, 0, Start, CheDb, Neves ).

chess_annotate_pgn_moves( [], _I, _Board, _CheDb, [] ).
% fixme: options for keeping old comments, for now dispose of them
chess_annotate_pgn_moves( [move(Mvn,Wht,Blc,_,_)|Mvs], I, Board, CheDb, [move(Mvn,Wht,Blc,Wcm,Bcm)|Nvs] ) :-

    chess_db:chess_db_handle( move, CheDb, Move ),
    chess_db:chess_db_handle( posi, CheDb, Posi ),

    chess_dict_inpo( Board, Inpo ),

    findall( GidX-Nmv, ( db_holds(Posi,game_posi(GidX,PlyX,Inpo)),
                         db_holds(Move,game_move(GidX,PlyX,_,Nmv))
                            ),
                                GMs ),
    kv_decompose( GMs, _, Played ),
    list_frequency( Played, RawFreqs ),
    kv_transpose( RawFreqs, FreqsRaw ),
    sort( FreqsRaw, AscFreqs ),
    reverse( AscFreqs, RepFreqs ),
    term_to_atom( RepFreqs, Wcm ),

    chess_dict_move( Wht, Board, Woard ),

    ( Blc == '' -> Noard = Woard
            ;

        chess_dict_inpo( Woard, BInpo ),
        findall( GidX-Nmv, ( db_holds(Posi,game_posi(GidX,PlyX,BInpo)),
                         db_holds(Move,game_move(GidX,PlyX,_,Nmv))
                            ),
                                BGMs ),
        kv_decompose( BGMs, _, BPlayed ),
        list_frequency( BPlayed, BRawFreqs ),
        kv_transpose( BRawFreqs, BFreqsRaw ),
        sort( BFreqsRaw, BAscFreqs ),
        reverse( BAscFreqs, BRepFreqs ),
        term_to_atom( BRepFreqs, Bcm ),
        chess_dict_move( Blc, Woard, Noard )
    ),
    J is I + 1,
    chess_annotate_pgn_moves( Mvs, J, Noard, CheDb, Nvs ).

kv_transpose( KVs, Trans ) :-
    findall( V-K, member(K-V,KVs), Trans ).
