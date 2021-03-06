
:- use_module(library(dcg/basics)).

/** pgn( +PgnF, -Pgn ).
    pgn( +PgnF, +Pgn ).
    pgn( +PgnIn, -PgnOut ).

Reads-from and writes-to between PGN files and Pgn term representations.<br>
It can also be used to ensure second argument is a Pgn term when either
a file or Pgn term is given as ground first argument.

A Pgn term is of the form: pgn(Info,Moves,Res,Orig). Where, <br>
  * Info is K-V pairs of the [] part of the PGN notation
  * Moves are the game moves: move(Num,Mv1,Mv2,Cms1,Cms2)
  * Res is the result (it usually also has an Info pair
  * Orig is the verbatim lines of the original .pgn input for that game

==
?- pgn( pgn('4NCL-2017'), Pgn ).  % in data/pgn
Pgn = [pgn(['Event'-'4NCL Division 1a', 'Site'-'Telford, ENG', 'Date'-'2017.11.11', ....] ...]

?- pgn( '18.03.10-Hampstead.pgn', Pgn ).
==

fixme: 
    1. if we have result from both Info and end of moves, make sure they are the same <br>
    2. if the info missing and the result from end of moves exists, then add it to info


@author nicos angelopoulos
@version  0.1 2018/03/14
@version  0.2 2018/08/01, changed from move(Num,Mvs,Cms), add $ starting NAGs, allow opening comment on game with moves
@tbd complement the 2 ways of getting the result.
@tbd   currently valuation marks and variations are thrown away

*/

pgn( File, Pgn ) :-
    ground( Pgn ),
    !,
    % absolute_file_name( Pgn, AbsPgn ),
    % pgn_write( AbsPgn, File ).
    absolute_file_name( File, AbsFile ),
    pgn_write( Pgn, AbsFile ).
pgn( PgnIn, PgnOut ) :-
    is_list( PgnIn ),
    !,
    PgnOut = PgnIn.
pgn( File, Pgn ) :-
    absolute_file_name( File, AbsFile, [access(exist),extensions([pgn,'PGN',''])] ),
    phrase_from_file(pgn_dcg(PgnPrv), AbsFile ),
    pgn_originals( AbsFile, Origs ),
    pgn_add_originals( PgnPrv, Origs, Pgn ).

pgn_write( Pgn, File ) :-
    open( File, write, Out ),
    write( the_pgn_var(Pgn) ), nl,
    maplist( pgn_write_stream(Out), Pgn ),
    close( Out ).

pgn_write_stream( Stream, pgn(Info,Moves,Res,_Orig) ) :-
    maplist( pgn_write_stream_info(Stream), Info ),
    nl( Stream ),
    pgn_write_stream_moves( Moves, 1, Stream ),
    write( Stream, Res ),
    nl( Stream ), nl( Stream ).

pgn_write_stream_info( Stream, Key-Value ) :-
    write( Stream, '[' ), write( Stream, Key ), write( Stream, ' "' ), 
    write( Stream, Value ), write( Stream, '"]' ), nl( Stream ).

pgn_write_stream_moves( [], _I, _Stream ).
pgn_write_stream_moves( [move(Mvn,Wht,Blc,Wcm,Bcm)|T], I, Stream ) :-
    % move(3, exd5, cxd5, ' [%clk 1:26:43] this is acomment', ' [%clk 1:31:43]'),
    write( Stream, Mvn ), write( Stream, '. ' ), write( Stream, Wht ), write( Stream, ' ' ),
    ( Wcm == '' -> true
        ; write( Stream, '{' ), write( Stream, Wcm ), write( Stream, '} ' )
    ),
    write( Stream, Mvn ), write( Stream, '... ' ), write( Stream, Blc ),
    write( Stream, ' ' ),
    ( Wcm == '' -> true
        ; write( Stream, '{' ), write( Stream, Bcm ), write( Stream, '} ' )
    ),
    ( 0 =:= (I mod 3) -> nl( Stream ); true ),
    J is I + 1,
    pgn_write_stream_moves( T, J, Stream ).

pgn_dcg( [H|T] ) -->
    pgn_dcg_game( H ),
    !,
    pgn_dcg( T ).
pgn_dcg( [] ) --> {true}.
    
pgn_dcg_game( pgn(Info,Moves,Res) ) --> 
    pgn_dcg_info( InfoPrv ),
    { debug_call( chess_db, chess_db:chess_db_info_kvs(InfoPrv) ) },
    pgn_dcg_moves( Moves, MovesInfo ),
    {append( InfoPrv, MovesInfo, Info )},
    pgn_dcg_result( Res ),
    pgn_dcg_move_end,
    !.

pgn_dcg_result( Res ) -->
    [32],
    !,
    pgn_dcg_result( Res ).
pgn_dcg_result( Res ) -->
    pgn_dcg_result_token( Res ),
    pgn_dcg_cntrl_m,
    [10].

pgn_dcg_result_token( '1-0' ) --> 
    "1-0", !.
pgn_dcg_result_token( '0-1' ) --> 
    "0-1", !.
pgn_dcg_result_token( '1/2-1/2' ) --> 
    "1/2-1/2", !.
pgn_dcg_result_token( '*' ) --> 
    "*".

pgn_dcg_moves( Moves, Info ) --> 
    pgn_dcg_ply_variation( CmStrings ),
    { CmStrings \== [] },
    { atomic_list_concat(CmStrings,'',Cm) },
    pgn_dcg_moves( Moves, TInfo ),
    {debug( chess_db(moves), 'Read moves: ~w', [Moves] )},
    {Info = ['Game_Note'-Cm|TInfo]}.

pgn_dcg_moves( Empty, AddToInfo ) --> 
    pgn_dcg_ply_variation( CmStrings ),
    { CmStrings \== [] },
    !,
    { atomic_list_concat(CmStrings,'',Cm),
      Empty = [], 
      AddToInfo = ['Game_Note'-Cm]
    }.
pgn_dcg_moves( Moves, Info ) -->
    pgn_dcg_moves_has( Moves ),
    !,
    {Info = []}.
pgn_dcg_moves( [], AddToInfo ) --> 
      { AddToInfo = ['Game_Note'-'No moves parsed (pgn/2).'] }.

pgn_dcg_moves_has( [move(Num,MvW,MvB,CmW,CmB)|T] ) -->
    integer( Num ),
    { debug( chess_db(move), 'Move: ~d', [Num] ) },
    ".", pgn_dcg_move_end,
    !,
    pgn_dcg_ply( MvW, CmW ),
    { debug( chess_db(move), 'Ply W: ~w, comment: ~w', [MvW,CmW] ) },
    pgn_dcg_variation, % is alternative always afer variation ?
    pgn_dcg_variation_white( Num ),
    { debug( chess_db(move), 'Variation done: ~d', [Num] ) },
    {MvW \= []},  % fixme: error
    pgn_dcg_ply( MvB, CmB ),
    { debug( chess_db(move), 'Ply B: ~w, comment: ~w', [MvB,CmB] ) },
    pgn_dcg_variation,
    % {append(MvW,MvB,Mvs)},
    % {append(CmW,CmB,Cms)},
    !,
    { debug( chess_db(move), 'Move read: ~w', [move(Num,MvW,MvB,CmW,CmB)] ) },
    % { (Num =:= 58, MvB = 'Qb4#') -> trace; true }, % here: use something like this, if your PGN fails to parse
    pgn_dcg_moves_has( T ).
pgn_dcg_moves_has( [] ) --> {true}.
    
pgn_dcg_ply( Mv, Cm ) -->
    [C],
    { \+ ( (0'0 =< C, C =< 0'9) ; (C =:= 0'*)) },
    !,
    string( String ),
    pgn_dcg_move_end,
    !,
    pgn_dcg_mark,
    { atom_codes( Mv, [C|String] )},
    pgn_dcg_ply_variation( CmStrings ),
    { atomic_list_concat(CmStrings,'',Cm) }.
pgn_dcg_ply( [], [] ) --> {true}.

pgn_dcg_ply_variation( [Str|Strs] ) -->
    [0'$],
    integer(Int),
    !,
    {atomic_list_concat(['$',Int],Str)},
    pgn_dcg_move_end,
    pgn_dcg_ply_variation( Strs ).
pgn_dcg_ply_variation( Strs ) -->
    [0'{],
    !,
    pgn_dcg_ply_variation_body( Strs ).
pgn_dcg_ply_variation( [] ) --> {true}.

pgn_dcg_ply_variation_body( [Str|Strs] ) -->
    string( StrCs ),
    pgn_dcg_ply_variation_segment_end( C ),
    !,
    {atom_codes(Str,StrCs)},
    pgn_dcg_ply_variation_continue( C, Strs ).
pgn_dcg_ply_variation_body( [] ) --> {true}.

pgn_dcg_ply_variation_continue( true, Strs ) -->
    pgn_dcg_ply_variation_body( Strs ).
pgn_dcg_ply_variation_continue( false, [] ) --> {true}.

pgn_dcg_ply_variation_segment_end( true ) --> 
    [13,10].
pgn_dcg_ply_variation_segment_end( true ) --> 
    [10].
pgn_dcg_ply_variation_segment_end( false ) --> 
    [0'}],
    pgn_dcg_move_end.

pgn_dcg_move_end --> " ", whites, pgn_dcg_skip_ends.
pgn_dcg_move_end --> [13,10], pgn_dcg_skip_ends.
pgn_dcg_move_end --> [10], pgn_dcg_skip_ends.

/*
pgn_dcg_skip_ends --> [13], !, pgn_dcg_skip_ends.
pgn_dcg_skip_ends --> [10], !, pgn_dcg_skip_ends.
pgn_dcg_skip_ends --> {true}.
*/
pgn_dcg_skip_ends --> pgn_dcg_move_end, !, pgn_dcg_skip_ends.
pgn_dcg_skip_ends --> {true}.

pgn_dcg_info( [] ) -->
    pgn_dcg_cntrl_m, [10].
pgn_dcg_info( [Key-Val|T] ) -->
    "[",
    string(KeyCs),
    " ",
         {atom_codes(Key,KeyCs)},
    !,
    "\"",
    string(ValCs),
    "\"",
    "]",
    !,
    {atom_codes(Val,ValCs)},
    % string( Next ),
    pgn_dcg_cntrl_m,
    [10],  % eol
    pgn_dcg_info( T ).
pgn_dcg_info( [] ) --> {true}. % allows for (incorrect?) luck of separation between info and moves

pgn_dcg_cntrl_m -->
    [13],
    !.
pgn_dcg_cntrl_m --> {true}.

pgn_dcg_variation --> 
    [0'(], 
    !,
    { debug(chess_db(move),'Parenthesis opening: ~d', 1) },
    pgn_dcg_variation_till_right(1),
    pgn_dcg_move_end,
    pgn_dcg_variation. % as there might multiple variations
pgn_dcg_variation --> {true}.

pgn_dcg_variation_till_right( 0 ) --> {!}.
pgn_dcg_variation_till_right( I ) --> 
    [0')],
    !,
    {H is I - 1},
    { debug(chess_db(move),'Parenthesis closing: ~d', I) },
    % pgn_dcg_move_end,
    pgn_dcg_variation_till_right( H ).
pgn_dcg_variation_till_right(I) --> 
    [0'(],
    !,
    {J is I + 1},
    { debug(chess_db(move),'Parenthesis opening: ~d', J) },
    pgn_dcg_variation_till_right( J ).
pgn_dcg_variation_till_right( I ) --> 
    [_],
    !,
    pgn_dcg_variation_till_right(I).

pgn_dcg_mark -->  % fixme: currently valuation marks (and variations) are thrown away
    [0'$],
    !,
    % pgn_dcg_mark_remainder,
    string( _ThrowAwayForNow ),
    pgn_dcg_move_end.
pgn_dcg_mark --> {true}.
   
pgn_dcg_variation_white( Num ) --> 
    integer( Num ),
    whites,% fixme: ends move codes
    [0'.,0'.,0'.],   %fixme: what ????!!!, this is surely a typo@18.10.08
    % whites.  % fixme: ends move codes
    !,
    pgn_dcg_move_end.
pgn_dcg_variation_white( _Num ) -->  {true}. % fixme: error

%%% dealing with originals (outside the parser)
% pgn_originals( FileR, [Fst|Origs] ) :-
pgn_originals( FileR, Origs ) :-
    % fixme: do the clean up way
    io_open( FileR, read, In ),
    io_line( In, Line ),
    % fixme, maybe you want to skip empty lines at top of file ?
    % pgn_original_till_next( Line, In, Next, Fst ),
    pgn_original_games( Line, In, Origs ),
    io_close( FileR, In ).

pgn_original_games( end_of_file, _In, [] ) :- !.
pgn_original_games( Line, In, [Orig|Origs] ) :-
    atom_codes( LineAtm, Line ),
    debug( chess_db(iline), 'next game starts at line: ~w', LineAtm ),
    pgn_original_game( Line, In, Next, Orig ),
    !,
    pgn_original_games( Next, In, Origs ).

pgn_original_game( Line, In, Next, Orig ) :-
    pgn_original_info( Line, In, Orig, ContInfo, ContOrig ),
    pgn_original_till_next( ContInfo, In, Next, ContOrig ).

pgn_original_info( [0'[|T], In, [[0'[|T]|Orig], ContInfo, ContOrig ) :-
    !,
    io_line( In, Line ),
    pgn_original_info( Line, In, Orig, ContInfo, ContOrig ).
pgn_original_info( ContLine, _In, ContOrig, ContLine, ContOrig ).

pgn_original_till_next( end_of_file, _In, end_of_file, [] ) :- !.
pgn_original_till_next( Other, In, Next, Lines ) :-
    io_line( In, Ahead ),
    pgn_original_till_next_ahead( Other, In, Ahead, Next, Lines ).

pgn_original_till_next_ahead( [], _In, Ahead, Next, Lines ) :-
    % io_line( In, Next ),
    ( Ahead = [0'[|_T] ; Ahead = end_of_file ),
    !,
    Next = Ahead,
    Lines = [[]].
pgn_original_till_next_ahead( [13], _In, Ahead, Next, Lines ) :-
    % io_line( In, Next ),
    ( Ahead = [0'[|_T] ; Ahead = end_of_file ),
    !,
    Next = Ahead,
    Lines = [[]].
pgn_original_till_next_ahead( Line, In, Ahead, Next, [Line|Lines] ) :-
    io_line( In, Follows ),
    pgn_original_till_next_ahead( Ahead, In, Follows, Next, Lines ).

% pgn_original_till_next( [0'[|T], _In, [0'[|T], [] ) :- !.
% pgn_original_till_next( Line, In, Ends, [Line|T] ) :-
    % io_line( In, Next ),
    % pgn_original_till_next( Next, In, Ends, T ).

pgn_add_originals( Pgns, Origs, Pgn ) :-
    length( Pgns, LenPgns ),
    length( Origs, LenOrigs ),
    ( LenPgns =:= LenOrigs -> true; 
                throw(pgns_originals_length_mismatch(LenPgns,LenOrigs) )
    ),
    pgn_add_originals_1( Pgns, Origs, Pgn ).

pgn_add_originals_1( [], [], [] ).
pgn_add_originals_1( [Pgn|Pgns], [Orig|Origs], [Ogn|Ogns] ) :-
    Pgn =.. [Name|Args],
    append( Args, [Orig], Orgs ),
    Ogn =.. [Name|Orgs],
    pgn_add_originals_1( Pgns, Origs, Ogns ).
