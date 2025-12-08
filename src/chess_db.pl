
chess_db_defaults( Defs ) :- 
                              Defs = [
                                             create(false),
                                             incr(false),
                                             incr_progress(1000),
                                             max_games(inf),
                                             position(true)
                                             ].

/** chess_db( +PgnOrF ).
     chess_db( +PgnOrF, +ChessDb ).
     chess_db( +PgnOrF, +Opts ).
     chess_db( +PgnOrF, +ChessDb, +Opts ).

Add games from a PGN file or a PGN term, PgnOrF, to the chess database 
pointed to by Db and/or Opts. If Db is given both as argument and option,
the argument overrides. If argument Db and/or option OptDb is a variable,
then the full location of the Db used is returned. 
If =|Create = false|=, PgnOrF points to a file and both Db and OptDb are variables,
then the stem of the PgnOrF is used as the name of Db. It is created in an existing directory
pointed to by a current alias defined in user:file_search_path/2- or the current directory
if no such alias exists.
To distinguish between the two arity 2 versions, Opts in that case need be a list.

Games in the database get a incremental unique id starting at 1 for first game.

Opts
  * create(Create=false)
     how to behave if ChessDb exists (see chess_db_connect/2)

  * db(OptDb)
     database location (see chess_db_connect/2)

  * dir(Dir)
     directory where database is located, (many allowed, see chess_db_connect/2)

  * incr(Incr=false)
     if =|true|= incrementally add a game at a time via a temporary file 
     (default is to read whole PGN into memory and then spit out the terms to the database)

  * incr_progress(IProg=1000)
     report (via debug(chess_db(true))) at every IProg games have been processed

  * max_games(MxG=inf)
    only process first MxG number of games- only enforced when =|Incr=true|=

  * position(Pos=true)
     if true, use position table

Options can also be picked up from ~/.pl/chess_db.pl (see options_append/3).

==
?- pgn( pgn('18.03-candidates'), Pgn ), 
   chess_db( Pgn, chess_db('18.03-candidates'), [db(Which),create(true)] ).
Pgn = ...
Which = '.../swipl-7.7.18/pack/chess_db_data/dbs/18.03-candidates'.

?- chess_db( pgn('4ncl_short.pgn'), fourNCL, [dir('/tmp'),create(true),db(Db)] ).
Db = '/tmp/fourNCL'.
==

@author nicos angelopoulos
@version  0.1 2018/3/14
@version  0.2 2018/8/17
@version  0.3 2025/12/8,  options incr(), incr_progress(), max_games()
@see options_append/3

*/
chess_db( PgnIn ) :-
     chess_db( PgnIn, _, [] ).

chess_db( PgnIn, DbOrOpts ) :-
     ( is_list(DbOrOpts) ->
          Db = _AbsDb,
          Opts = DbOrOpts
          ;
          Db = DbOrOpts,
          Opts = []
     ),
     chess_db( PgnIn, Db, Opts ).

chess_db( PgnIn, ArgDb, Args ) :-
     options_append( chess_db, Args, Opts ),
     ( memberchk(db(OptDb),Opts) -> true; true ),
     % options( create(Create), Opts ),
     % options( position(Pos), Opts ),
     ( ground(ArgDb) -> PrvDb = ArgDb; PrvDb = OptDb ),
     % ( var(Db) -> throw(variable_location(ArgDb,Args)); true ),
     ( var(PrvDb) -> 
            absolute_file_name(PgnIn,AbsPgnF), % if you ever add options to pgn/2 abs_file could be one of them. 
            os_base( AbsPgnF, BaseF ),
            os_ext( _, Stem, BaseF ),
            ( chess_db_alias_exists_dir(AliasDir) ->
                os_path( AliasDir, Stem, Db )
                ;
                Db = Stem
            )
            % throw(variable_location(ArgDb,Args));
            ; 
            Db = PrvDb
     ),
     chess_db_connect( Db, [db(AbsDb),handles(CdbHs)|Opts] ),
     % chess_db_handles( Create, Pos, Dir, CdbHs, AbsDb ),
     chess_db_handle( info, CdbHs, InfoHandle ),
     chess_db_max_id( InfoHandle, LaGid ),
     options( incr(Incr), Opts ),
     options( incr_progress(IProg), Opts ),
     options( max_games(MxG), Opts ),
     OfG is LaGid + MxG,
     chess_db_incr( Incr, PgnIn, LaGid, OfG, IProg, CdbHs, AbsDb, ArgDb, OptDb ).

chess_db_incr( false, PgnIn, LaGid, _MxG, _IProg, CdbHs, AbsDb, ArgDb, OptDb ) :-
     pgn( PgnIn, Pgn ),
     chess_db_games_add( Pgn, LaGid, CdbHs ),
     ( atomic(AbsDb) -> chess_db_disconnect(AbsDb); true ),
     % chess_db_handles_close( CdbHs ),
     ( var(ArgDb) -> ArgDb = AbsDb; true ),
     ( var(OptDb) -> OptDb = AbsDb; true ).
chess_db_incr( true, PgnIn, LaGid, MxG, IProg, CdbHs, AbsDb, ArgDb, OptDb ) :-
     open( PgnIn, read, Pin ),
     tmp_file( chess_db_tmp, TmpF ),
     chess_db_incr_stream( Pin, TmpF, LaGid, MxG, IProg, CdbHs ), 
     close( Pin ),
     ( atomic(AbsDb) -> chess_db_disconnect(AbsDb); true ),
     % chess_db_handles_close( CdbHs ),
     ( var(ArgDb) -> ArgDb = AbsDb; true ),
     ( var(OptDb) -> OptDb = AbsDb; true ).

chess_db_incr_stream( Pin, TmpF, LaGid, MxG, IProg, CdbHs ) :-
     open( TmpF, write, TempO ),
     chess_db_incr_stream_pgn( Pin, TempO, Eof ),
     close( TempO ),
     ( (Eof ; (MxG =< LaGid) ) -> Termin = true; Termin = false ),
     chess_db_incr_stream_termin( Termin, Pin, TmpF, LaGid, MxG, IProg, CdbHs ).

chess_db_incr_stream_termin( true, _Pin, _TmpF, _LaGid, _MxG, _IProg, _CdbHs ).
chess_db_incr_stream_termin( false, Pin, TmpF, LaGid, MxG, IProg, CdbHs ) :-
     pgn( TmpF, Game ),
     chess_db_games_add( Game, LaGid, CdbHs ),
     MaGid is LaGid + 1,
     ( (LaGid mod IProg) =:= 0 ->
               debuc( chess_db(true), task(stop), 'Added game no: ~d', [farg(MaGid)] )
               ;
               true
     ),
     chess_db_incr_stream( Pin, TmpF, MaGid, MxG, IProg, CdbHs ).

chess_db_incr_stream_pgn( Pin, TempO, Eof ) :-
     io_line( Pin, Line ),
     chess_db_incr_stream_pgn( Line, Pin, TempO, Eof ).

chess_db_incr_stream_pgn( end_of_file, _Pin, _TempO, Eof ) :-
     !,
     Eof = true.
chess_db_incr_stream_pgn( [], Pin, TempO, Eof ) :- % ignore ?
     io_line( Pin, Line ),
     chess_db_incr_stream_pgn( Line, Pin, TempO, Eof ).
chess_db_incr_stream_pgn( [0'[|Tail], Pin, TempO, Eof ) :-
     !,
     io_line( TempO, [0'[|Tail] ),
     io_line( Pin, InfoL ),
     chess_db_incr_stream_pgn_info( InfoL, Pin, TempO, Eof ).
chess_db_incr_stream_pgn( Line, _Pin, _TempO, _Eof ) :-
     atom_codes( Atm, Line ),
     throw( non_info_line(Atm) ).

chess_db_incr_stream_pgn_info( [0'[|Tail], Pin, TempO, Eof ) :-
     !,
     io_line( TempO, [0'[|Tail] ),
     io_line( Pin, InfoL ),
     chess_db_incr_stream_pgn_info( InfoL, Pin, TempO, Eof ).
chess_db_incr_stream_pgn_info( [], Pin, TempO, Eof ) :-
     !,
     io_line( TempO, [] ),
     io_line( Pin, MoveL ),
     chess_db_incr_stream_pgn_move( MoveL, Pin, TempO, Eof ).
chess_db_incr_stream_pgn_info( Line, _Pin, _TempO, _Eof ) :-
     atom_codes( Atm, Line ),
     throw( non_info_line_1(Atm) ).

chess_db_incr_stream_pgn_move( end_of_file, _Pin, _TempO, Eof ) :-
     Eof = false.
chess_db_incr_stream_pgn_move( [], _Pin, TempO, Eof ) :-
     !,
     io_line( TempO, [] ),
     Eof = false.
chess_db_incr_stream_pgn_move( Line, Pin, TempO, Eof ) :-
     io_line( TempO, Line ),
     io_line( Pin, MoveL ),
     chess_db_incr_stream_pgn_move( MoveL, Pin, TempO, Eof ).

chess_db_alias_exists_dir( AliasDir ) :-
     user:file_search_path( chess_db, AliasDir ),
     exists_directory( AliasDir ),
     !.

chess_db_games_add( [], _Gid, _CdbHs ).
chess_db_games_add( [G|Gs], Gid, CdbHs ) :-
     % fixme: ignore position for now
     G = pgn(Info,Moves,_Res,Orig),
     chess_db_debug_info( Info, 'adding to db' ),
     chess_db_handle( info, CdbHs, InfoHandle ),
     chess_db_handle( move, CdbHs, MoveHandle ),
     chess_db_handle( orig, CdbHs, OrigHandle ),
     chess_db_handle( posi, CdbHs, PosiHandle ),
     chess_db_game_add( InfoHandle, Info, Moves, Orig, Gid, MoveHandle, OrigHandle, PosiHandle, Nid ),
     chess_db_games_add( Gs, Nid, CdbHs ).

chess_db_debug_info( Info, Pfx ) :-
   memberchk( 'White'-White, Info ),
   memberchk( 'Black'-Black, Info ),
   atomic_list_concat( [White,vs,Black], ' ', Mess ),
   !,
   debug( chess_db, '~w: ~w', [Pfx,Mess] ).
chess_db_debug_info( Info, Pfx ) :-
   debug( chess_db, '~w: ~w', [Pfx,Info] ).

chess_db_game_add( InfoHandle, Info, _Moves, _Orig, Gid, _MoHa, _OrHa, _PoHa, Gid ) :-
     chess_db_game_info_exists( Info, InfoHandle, ExGid ),
     !, % fixme: add option for erroring
     debug( chess_db(info), 'Info match existing game: ~d', ExGid ).
chess_db_game_add( InfoHandle, Info, Moves, Orig, Gid, MoHa, OrHa, PoHa, Nid ) :-
     Nid is Gid + 1,
     findall( game_info(Nid,K,V), member(K-V,Info), Goals ),
     db_assert( InfoHandle, Goals, _ ),
     chess_dict_start_board( Start ),
     chess_pgn_moves_limos( Moves, 1, Start, Limos ),
     findall( game_move(Nid,Ply,Hmv,NxtMv), member(limo(Ply,Hmv,NxtMv,_Inpo),Limos), Moals ),
     db_assert( MoHa, Moals, _ ),
     chess_db_limos_game_posi( Limos, Nid, PoHa ),
     maplist( atom_codes, OrigAtms, Orig ),
     atomic_list_concat( OrigAtms, '\n', OrigAtm ),
     debug( chess_db(original), '~a', OrigAtm ),
     db_assert( OrHa, game_orig(Nid,OrigAtm), _ ).

/** chess_db_limos_game_posi( +Limos, +Gid, +Db ).

     Add a number of positions from Limos structures from game Gid, on to position table with db handle PoHa.

*/
chess_db_limos_game_posi( [], _Gid, _PosDb ).
chess_db_limos_game_posi( [limo(Ply,_Hmv,Mv,Inpo)|T], Gid, PosDb ) :-
     % write( mv(Mv) ), nl,
     % ( Mv == 'c4' -> trace; true ),
     ( Mv == [] ->
          true
          ; 
          atomic_list_concat( [Gid,Ply,Mv], '-', This ),
          ( (db_holds(PosDb,game_posi(Inpo,Curr)),db_retractall(PosDb,game_posi(Inpo,_),_) ) -> 
               atomic_list_concat( [Curr,This], ';', Next )
               ;
               Next = This
          ),
          % ( integer(Inpo) -> InpoInt = Inpo; atom_number(Inpo,InpoInt) ),
          db_assert( PosDb, game_posi(Inpo,Next), _ )
     ),
     chess_db_limos_game_posi( T, Gid, PosDb ).

chess_db_game_info_exists( [], _InfHa, _ExGid ).
chess_db_game_info_exists( [K-V|T], InfHa, ExGid ) :-
     db_holds( InfHa, game_info(ExGid,K,V) ),
     chess_db_game_info_exists( T, InfHa, ExGid ).

chess_db_table_fields( game_info, [gid+integer,key+text,value-text] ).
% chess_db_table_fields( game_move, [gid+integer,move_no+integer,turn+boolean,move-text] ).
chess_db_table_fields( game_move, [gid+integer,ply+integer,hmv-integer,move-text] ).
chess_db_table_fields( game_orig, [gid+integer,original-text] ).
% chess_db_table_fields( game_posi, [gid+integer,ply+integer,position-text] ).
chess_db_table_fields( game_posi, [position+text,pairs-text] ).

chess_db_dir( Dir, _Create, AbsDir ) :-
     AbsOpts = [file_type(directory),file_errors(fail)], % fixme: assumes dir-based db
     absolute_file_name( Dir, AbsDir, AbsOpts ),
     % exists_directory( Dir ),
     !,
     % fixme: can the following into a debug_call(,dir,chess_db/Dir).
     debug( chess_db(info), 'Using existing chess_db directory: ~w', AbsDir ).
chess_db_dir( Dir, Create, AbsDir ) :-
     Create == true,
     % AbsOpts = [solutions(first),file_errors(fail),mode(none),expand(true)],
     chess_db_dir_create( Dir, AbsDir ),
     !,
     debug( chess_db(info), 'Creating new chess_db directory: ~w', AbsDir ),
     make_directory_path( AbsDir ).
chess_db_dir( Dir, Create, Dir ) :-
     throw( chess_db_dir_does_not_exist_and_asked_not_to_create_it_by(Dir,Create) ).

chess_db_dir_create( Dir, AbsDir ) :-
     absolute_file_name( Dir, AbsDir, [solutions(all)] ),
     % when creating prefer aliases that exist...
     ( (compound(Dir),functor(Dir,Name,1)) -> 
        user:file_search_path( Name, _Path ),
        Mock =.. [Name,what],
        absolute_file_name( Mock, AbsMock, [solutions(all)] ),
        directory_file_path( NamePath, what, AbsMock ),
        atom_concat(NamePath,_,AbsDir), 
        exists_directory(NamePath),
        debug( chess_db(info), 'New chess_db directory (existing search): ~w', [AbsDir] )
        ;
        true
     ),
     !.
chess_db_dir_create( Dir, AbsDir ) :-
     absolute_file_name( Dir, AbsDir, [solutions(all)] ),
     absolute_file_name( pack(chess_db), AbsChDb ),
     directory_file_path( Prefix, chess_db, AbsChDb ),
     atom_concat( Prefix, _, AbsDir ),
     !,
     debug( chess_db(info), 'New chess_db directory (pack prefix): ~w', [AbsDir] ).
chess_db_dir_create( Dir, AbsDir ) :-
     debug( chess_db(info), 'New chess_db directory (fall back): ~w', [AbsDir] ),
     absolute_file_name( Dir, AbsDir ).
