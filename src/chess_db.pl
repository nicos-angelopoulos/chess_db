
chess_db_defaults( Defs ) :- 
                              Inf is inf,
                              Defs = [
                                             bests_limit(10),
                                             create(false),
                                             close(false),
                                             goal(chess_db_games_add),
                                             goal_iter(_),
                                             goal_return(_),
                                             handles(_),
                                             incr(false),
                                             progress(1000),
                                             max_games(inf),
                                             position(true),
                                             position_depth_limit(Inf),
                                             position_type(kvx),
                                             check_types(Types)
                                             ],
                              Types = [
                                        close-boolean,
                                        create-one_of([true,false,new,fresh]),
                                        incr-boolean,
                                        progress-integer,
                                        position-boolean
                                     ].

/** chess_db( +PgnOrF ).
     chess_db( +PgnOrF, +ChessDb ).
     chess_db( +PgnOrF, +Opts ).
     chess_db( +PgnOrF, +ChessDb, +Opts ).

Process games from a PGN list of terms or .pgn file. 

By default games are added to current chess database pointed b ChessDb or Opts. 
As of v0.2 user define "processors" can be defined via option goal(Goal).

If both ChessDb and OptDb are given, ChessDb overrides.
If argument ChessDb and/or option OptDb is a variable, then the full location of the Db used is returned. 
If =|Create = false|=, PgnOrF points to a file/dir and both Db and OptDb are variables,
then the stem of the PgnOrF is used as the name of Db. It is created in an existing directory
pointed to by a current alias defined in user:file_search_path/2- or the current directory
if no such alias exists.
To distinguish between the two arity 2 versions, Opts in that case needs to be a list.

Games in the database get a incremental unique id starting at 1 for first game.

If ChessDB was not connected previously, it will after the call, and it will remain so except if close(true) is passed in options.

As of v0.4 we can process PGNs in otherways, than adding to a database. 
This is via defining Goal (goal() option). The goal will be called as
==
     ?- Goal( Pgn, Giter, Progr, Pos, Tos, CdbHs, Niter ).
               call( Goal, Game, LaGid, IProg, Posi, Rosi, CdbHs, NxGid )

     Pgn   = list of PGN terms
     Giter= Giter (input)
     Prg  = option progress(Prg)      (invariant)
     Pos  = option position(Pos)      (invariant)
     Tos  = option position_type(Tos) (invariant)
     CdbHs= db handles; will be unbound variable if =|Giter \== chess_db_games_add|=
     Niter= next Giter (output)
==

Opts
  * bests_limit(Bim=10)
    limit of how many best game identifiers to keep for each position

  * create(Create=false)
     how to behave if ChessDb exists (see chess_db_connect/2)

  * close(Close=false)
     whether to close the connection to ChessDB after adding the games

  * db(OptDb)
     database location (see chess_db_connect/2)

  * dir(Dir)
    directory where database is located, (many allowed, see chess_db_connect/2)

  * goal(Goal=chess_db_games_add)
    Goal for processing read-in game terms. The default adds them to database.
    As of v0.4 alternatives are possible. See comment below.

  * goal_iter(Giter)
    Goal iterator, can be left unbound when addiing games to DB- in which case itwill become the 
    game id, instantiated by the predicate to the last integer game id in the database we adding to.
    When user specific Goal is given, this can be used to pass information to it.

  * goal_return(Gret)
    returns the final value of Giter

  * handles(CdbHs)
    handles for the databases(s) opened (as per chess_db_connect/2).

  * incr(Incr=false)
    if =|true|= incrementally add a game at a time via a temporary file 
    (default is to read whole PGN into memory and then spit out the terms to the database)

  * progress(IProg=1000)
    report (via debug(chess_db(true))) at every IProg games have been processed

  * max_games(MxG=inf)
    only process first MxG number of games- only enforced when =|Incr=true|=

  * position(Pos=true)
    if true, use position table
     
  * position_depth_limit(Pdl=inf)
    number of first moves (-1) for which positions are kept for each game. 
    Decimals of the form .5 are allowed, 10.5 means keep postions until 10th white move (no black 10th).
    Slightly counter intuitively, =|Pdl=:=10|= will save positions for moves 1-10 inclusive of black move 10
    (ie one more position than when =|pdf=:=10.5|=).

  * position_type(Pos=kvx)
    type of the position table representation
    * kvx kv value where v is text

  * table(Tbl[,DbType])
    if present only update/generate these tables (multiple allowed).
    By default all 4 tables are updated/created. If DbType is not given the default is assumed 
    (which is rocksdb if pack(rocksdb) is installed and sqlite otherwise- requires pack(prosqlite)).

Options can also be picked up from ~/.pl/chess_db.pl (see options_append/3).

By default the predicate updates generates 4 tables
  * info
  * move
  * orig
  * posi

See pack doc chess_db for details.

You can chose which tables to generate/update using table/1,2 option. If one is given, then 
all tables should be given explicitly. 

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
@version  0.3 2025/12/8,  options incr(), progress(), max_games()
@version  0.4 2026/1/23,  options goal(), goal_{iter,return}(), bests_limit(), position_depth_limit()
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
     options( position(Posi), Opts ),
     options( position_type(Rosi), Opts ),
     options( goal(Goal), Opts ),
     options( goal_iter(Gitr), Opts ),
     chess_db_set_up_handles( Goal, Gitr, PgnIn, AbsDb, ArgDb, OptDb, CdbHs, Opts ),
     options( handles(CdbHs), Opts ),
     options( incr(Incr), Opts ),
     options( progress(IProg), Opts ),
     options( position_depth_limit(Mly), Opts ),
     ( Mly =:= inf -> Dly is inf
          ; ( integer(Mly) -> 
                    Dly is Mly * 2
                    ;
                    Dly is integer( ((Mly - 1) * 2) )
            )
     ),
     debuc( chess_db(true), 'Ply limit for positions table: ~w', [Dly] ),
     options( bests_limit(Bim), Opts ),
     options( max_games(MxG), Opts ),
     ( MxG =:= inf -> OfG is inf ; ( number(Gitr) -> OfG is Gitr + MxG; MxG is inf) ),
     debuc( chess_db(true), task(start), 'PGN load from: ~w', [farg(PgnIn),pred(chess_db/2)] ),
     debuc( chess_db(true), option, incr(Incr), pred(chess_db/2) ),
     debug( chess_db(stats), 'Stats channel is on.', [] ),
     chess_db_incr( Incr, PgnIn, Goal, Gitr, OfG, IProg, Posi, Rosi, Dly, Bim, CdbHs, AbsDb, ArgDb, OptDb, RtGid ),
     DbcOpts = [check_point(finished_at_id(RtGid)),comment(false)],
     debuc( chess_db(stats), stat, cputime, DbcOpts ),
     debuc( chess_db(stats), stat, process_cputime, DbcOpts ),
     debuc( chess_db(stats), stat, real_time, DbcOpts ),
     debuc( chess_db(stats), stat, runtime, DbcOpts ),
     debuc( chess_db(stats), stat, system_time, DbcOpts ),
     options( close(Close), Opts ),
     chess_db_close( Close, AbsDb, CdbHs ),
     options( goal_return(RtGid), Opts ).

chess_db_set_up_handles( chess_db_games_add, Gitr, PgnIn, AbsDb, ArgDb, OptDb, CdbHs, Opts ) :-
     !,
     ( memberchk(db(OptDb),Opts) -> true; true ),
     ( ground(ArgDb) -> PrvDb = ArgDb; PrvDb = OptDb ),
     ( var(PrvDb) ->
            absolute_file_name(PgnIn,AbsPgnF), % if you ever add options to pgn/2 abs_file could be one of them. 
            os_base( AbsPgnF, BaseF ),
            os_ext( _, Stem, BaseF ),
            ( chess_db_alias_exists_dir(AliasDir) ->
                os_path( AliasDir, Stem, Db )
                ;
                Db = Stem
            )
            ; 
            Db = PrvDb
     ),
     chess_db_connect( Db, [db(AbsDb)|Opts] ),
     options( handles(CdbHs), Opts ),
     chess_db_handle( info, CdbHs, InfoHandle ),
     chess_db_max_id( InfoHandle, Gitr ).
chess_db_set_up_handles( _Goal, _Gitr, _PgnIn, AbsDb, ArgDb, OptDb, CdbHs, _Opts ) :-
     AbsDb = null,
     ArgDb = null,
     OptDb = null,
     CdbHs = null.

chess_db_close(false, _AbsDb, _CdbHs).
chess_db_close( true, AbsDb, _CdbHs ) :-
     chess_db_disconnect( AbsDb ).

chess_db_incr( false, PgnIn, Goal, LaGid, _MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, AbsDb, ArgDb, OptDb, RtGid ) :-
     pgn( PgnIn, Pgn ),
     DbcOpts = [check_point(start_at_id(LaGid)),comment(false)],
     debuc( chess_db(stats), stat, cputime, DbcOpts ),
     debuc( chess_db(stats), stat, process_cputime, DbcOpts ),
     debuc( chess_db(stats), stat, real_time, DbcOpts ),
     debuc( chess_db(stats), stat, runtime, DbcOpts ),
     debuc( chess_db(stats), stat, system_time, DbcOpts ),
     ( Goal == chess_db_games_add ->
          chess_db_games_add( Pgn, LaGid, IProg, Posi, Rosi, Dly, Bim, CdbHs, RtGid ),
          ( var(ArgDb) -> ArgDb = AbsDb; true ),
          ( var(OptDb) -> OptDb = AbsDb; true )
          ;
          call( Goal, Pgn, LaGid, IProg, Posi, Rosi, CdbHs, RtGid )
     ).
chess_db_incr( true, PgnIn, Goal, LaGid, MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, AbsDb, ArgDb, OptDb, RtGid ) :-
     open( PgnIn, read, Pin ),
     tmp_file( chess_db_tmp, TmpF ),
     DbcOpts = [check_point(start_at_id(LaGid)),comment(false)],
     debuc( chess_db(stats), stat, cputime, DbcOpts ),
     debuc( chess_db(stats), stat, process_cputime, DbcOpts ),
     debuc( chess_db(stats), stat, real_time, DbcOpts ),
     debuc( chess_db(stats), stat, runtime, DbcOpts ),
     debuc( chess_db(stats), stat, system_time, DbcOpts ),
     chess_db_incr_stream( Pin, TmpF, Goal, LaGid, MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, RtGid ), 
     close( Pin ),
     % ( atomic(AbsDb) -> chess_db_disconnect(AbsDb); true ),
     % chess_db_handles_close( CdbHs ),
     ( var(ArgDb) -> ArgDb = AbsDb; true ),
     ( var(OptDb) -> OptDb = AbsDb; true ).

chess_db_incr_stream( Pin, TmpF, Goal, LaGid, MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, RtGid ) :-
     open( TmpF, write, TempO ),
     chess_db_incr_stream_pgn( Pin, TempO, Eof ),
     close( TempO ),
     ( (Eof ; (number(LaGid),MxG =< LaGid) ) -> Termin = true; Termin = false ),
     chess_db_incr_stream_termin( Termin, Pin, TmpF, Goal, LaGid, MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, RtGid ).

chess_db_incr_stream_termin( true, _Pin, _TmpF, _Goal, LaGid, _MxG, _IProg, _Posi, _Rosi, _Dly, _Bim, _CdbHs, RtGid ) :-
     LaGid = RtGid.
chess_db_incr_stream_termin( false, Pin, TmpF, Goal, LaGid, MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, RtGid ) :-
     pgn( TmpF, Game ),
     ( Goal == chess_db_games_add ->
               chess_db_games_add( Game, LaGid, IProg, Posi, Rosi, Dly, Bim, CdbHs, NxGid )
               ;
               call( Goal, Game, LaGid, IProg, Posi, Rosi, CdbHs, NxGid )
     ),
     % MaGid is LaGid + 1,
     chess_db_incr_stream( Pin, TmpF, Goal, NxGid, MxG, IProg, Posi, Rosi, Dly, Bim, CdbHs, RtGid ).

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
chess_db_incr_stream_pgn_info( [13], Pin, TempO, Eof ) :-
     !,
     io_line( TempO, [] ),
     io_line( Pin, MoveL ),
     chess_db_incr_stream_pgn_move( MoveL, Pin, TempO, Eof ).
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
chess_db_incr_stream_pgn_move( [13], _Pin, TempO, Eof ) :-
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

chess_db_games_add( [], Gid, _IProg, _Posi, _Rosi, _Dly, _Bim, _CdbHs, Xid ) :-
     Gid = Xid.
chess_db_games_add( [G|Gs], Gid, IProg, Posi, Rosi, Dly, Bim, CdbHs, Xid ) :-
     % fixme: ignore position for now
     G = pgn(Info,Moves,Res,Orig),
     chess_db_debug_info( Info, 'adding to db' ),
     chess_db_handle( info, CdbHs, InfoHandle ),
     chess_db_handle( move, CdbHs, MoveHandle ),
     chess_db_handle( orig, CdbHs, OrigHandle ),
     ( Posi == true -> chess_db_handle( posi, CdbHs, PosiHandle ) ; true ),
     chess_db_game_add( InfoHandle, Info, Moves, Orig, Dly, Bim, Gid, Res, MoveHandle, OrigHandle, IProg, Posi, Rosi, PosiHandle, Nid ),
     chess_db_games_add( Gs, Nid, IProg, Posi, Rosi, Dly, Bim, CdbHs, Xid ).

chess_db_debug_info( Info, Pfx ) :-
     memberchk( 'White'-White, Info ),
     memberchk( 'Black'-Black, Info ),
     atomic_list_concat( [White,vs,Black], ' ', Mess ),
     !,
     debug( chess_db, '~w: ~w', [Pfx,Mess] ).
chess_db_debug_info( Info, Pfx ) :-
     debug( chess_db, '~w: ~w', [Pfx,Info] ).

chess_db_game_add( InfoHandle, Info, _Moves, _Orig, _Dly, _Bim, Gid, _Res, _MoHa, _OrHa, _IProg, _Posi, _Rosi, _PoHa, Gid ) :-
     chess_db_game_info_exists( Info, InfoHandle, ExGid ),
     !, % fixme: add option for erroring
     debug( chess_db(info), 'Info match existing game: ~d', ExGid ).
chess_db_game_add( InfoHandle, Info, Moves, Orig, Dly, Bim, Gid, Res, MoHa, OrHa, IProg, Posi, Rosi, PoHa, Nid ) :-
     Nid is Gid + 1,
     chess_db_game_add_info( InfoHandle, Info, Nid ),
     chess_dict_start_board( Start ),
     chess_pgn_moves_limos( Moves, 1, Start, Limos ),
     chess_db_limos_game_moves( MoHa, Nid, Limos ),
     ( Posi == true ->
          chess_db_res_index( Res, Rex ),
          chess_db_game_info_elo( 'WhiteElo', Info, WhiteELO ),
          chess_db_game_info_elo( 'BlackElo', Info, BlackELO ),
          ELO is WhiteELO + BlackELO,
          chess_db_limos_game_posi( Limos, Dly, Bim, Nid, ELO, Rex, Rosi, PoHa )
          ;
          true
     ),
     maplist( atom_codes, OrigAtms, Orig ),
     atomic_list_concat( OrigAtms, '\n', OrigAtm ),
     debug( chess_db(original), '~a', OrigAtm ),
     chess_db_inc_id( InfoHandle, Nid ),
     ( (Nid mod IProg) =:= 0 ->
               DbcOpts = [check_point(added_id(Nid)),comment(false)],
               debuc( chess_db(stats), stat, cputime, DbcOpts ),
               debuc( chess_db(stats), stat, process_cputime, DbcOpts ),
               debuc( chess_db(stats), stat, real_time, DbcOpts ),
               debuc( chess_db(stats), stat, runtime, DbcOpts ),
               debuc( chess_db(stats), stat, system_time, DbcOpts ),
               debuc( chess_db(true), task(stop), 'Added game no: ~d', [farg(Nid)] )
               ;
               true
     ),
     chess_db_table_update( game_orig(Nid,OrigAtm), OrHa).

chess_db_game_info_elo( Key, Info, Elo ) :-
     ( memberchk(Key-EloPrv,Info) ->
               (number(EloPrv) -> Elo = EloPrv; (catch(atom_number(EloPrv,Elo),_,fail) -> true; Elo is 0))
               ;
               Elo is 0
     ).
          
/** chess_db_limos_game_posi( +Limos, +Dly, +Bim, +Gid, +Rex, +Db ).

     Add a number of positions from Limos structures from game Gid, on to position table with db handle PoHa.

     Rex is the result index 1-> white win, 2-> draw, 3-> black win, 4-> undetermined

     Dly is the depth ply.

     Bim is the bests limit.

     25.12.09: new implementation. we plan to split this to 3 tables, 
               testing implementation of first. 
               The (new) integer position points to an atom/string of the form
                    [w:d:b;]M1:#G1-w:d:b;M2...
                    * Mov1:w:d:b;Mov2... [this assumes all games have a known result...]

     25.12.18
          the second table will contain unique sequences by which position has been arrived 
          that way all games can potentially be traced

          the third table will store the Gid and sum of Elos of the players of top N games that reached this position

@author nicos angelopoulos
@version  0:2 2025/12/09
@tbd how are games that finish at a position are represented ?
@tbd some code to ensure that e4+ and e4 are the same ?
     
*/
chess_db_limos_game_posi( [], _Dly, _Bim, _Gid, _ELO, _Rex, _Rosi, _PosDb ).
chess_db_limos_game_posi( [limo(Ply,_Hmv,Mv,Inpo)|T], Dly, Bim, Gid, ELO, Rex, Rosi, PosDb ) :-
     % write( mv(Mv) ), nl,
     % ( Mv == 'c4' -> trace; true ),
     ( Mv == [] ->
          true
          ; 
          ( chess_db_holds(game_posi(Rosi),PosDb,[Inpo,Mv],Curr) ->
                    chess_db_posi_value_update( Rosi, Curr, Bim, Gid, ELO, Rex, Mv, Next )
                    ;
                    chess_db_posi_value_create( Rosi, Bim, Gid, ELO, Rex, Mv, Next )
          ),
          chess_db_table_update( game_posi(Rosi), PosDb, [Inpo,Mv], Next )
     ),
     ( Dly =< Ply -> Rest = []; Rest = T ),
     chess_db_limos_game_posi( Rest, Dly, Bim, Gid, ELO, Rex, Rosi, PosDb ).

/*
chess_db_posi_value_update( Rosi, Curr, Mv, Next )
          % atomic_list_concat( [Gid,Ply,Mv], '-', This ),
          ( (db_holds(PosDb,game_posi(Inpo,Curr)),db_retractall(PosDb,game_posi(Inpo,_),_) ) -> 
               % atomic_list_concat( [Curr,This], ';', Next )
               atomic_list_concat( Conts, ';', Curr ),
               % this doesn't fail if there is a problem
               findall( MvX-res(WX,DX,BX), (member(Cont,Conts),atomic_list_concat([MvX,WX,DX,BX],':',Cont)), MDprs ),
               ( select(Mv-res(Ws,Ds,Bs),MDprs,RMprs) ->
                    chess_db_inc_res_index( Rex, res(Ws,Ds,Bs), NxRes )
                    ;
                    RMprs = MDprs,
                    chess_db_inc_res_index( Rex, res('0','0','0'), NxRes )
               ),
               NXprs = [Mv-NxRes|RMprs],
               findall( ACont, (member(MvY-res(WY,DY,BY),NXprs),atomic_list_concat([MvY,WY,DY,BY],':',ACont)), NxConts ),
               atomic_list_concat( NxConts, ';', Next )
               ;
               % Next = This
               chess_db_inc_res_index( Rex, res('0','0','0','0'), res(WN,DN,BN,UN) ),
               atomic_list_concat( [Mv,WN,DN,BN,UN], ':', Next )
          ),
          % ( integer(Inpo) -> InpoInt = Inpo; atom_number(Inpo,InpoInt) ),
          db_assert( PosDb, game_posi(Inpo,Next), _ )
     ),
     chess_db_limos_game_posi( T, Gid, Rex, PosDb ).
     */

chess_db_posi_value_update( kvx, Curr, Bim, Gid, ELO, Rex, Mv, Next ) :-  % kvx: Key-Val where value is a text
     atomic_list_concat( Parts, ';', Curr ),
     once( append(Conts,[BestsAtm],Parts) ),
     chess_db_posi_value_kvx_update_best( BestsAtm, Bim, Gid, ELO, BestsNx ),
     % this doesn't fail if there is a problem
     findall( MvX-res(WX,DX,BX,UX), (member(Cont,Conts),atomic_list_concat([MvX,WX,DX,BX,UX],':',Cont)), MDprs ),
     ( select(Mv-res(Ws,Ds,Bs,Us),MDprs,RMprs) ->
          chess_db_inc_res_index( Rex, res(Ws,Ds,Bs,Us), NxRes )
          ;
          RMprs = MDprs,
          chess_db_inc_res_index( Rex, res('0','0','0','0'), NxRes )
     ),
     NXprs = [Mv-NxRes|RMprs],
     findall( ACont, (member(MvY-res(WY,DY,BY,UY),NXprs),atomic_list_concat([MvY,WY,DY,BY,UY],':',ACont)), NxConts ),
     append( NxConts, [BestsNx], NxParts ),
     atomic_list_concat( NxParts, ';', Next ).

chess_db_posi_value_kvx_update_best( Bests, Bim, Gid, ELO, Nexts ) :-
     atomic_list_concat( [NoCurrAtm,LstELOAtm,LstGid|Parts], ':', Bests ),
     % fixme: allow Lim as input/parameter
     % Lim is 10,
     atom_number( GidAtm, Gid ),
     ( (nth1(N1,[LstGid|Parts],GidAtm),odd(N1)) -> 
               Nexts = Bests
               ;
               atom_number( NoCurrAtm, NoCurr ),
               atom_number( LstELOAtm, LstELO ),
               compare( Op, LstELO, ELO ),
               chess_db_posi_value_kvx_update_best_op( Op, NoCurr, Bim, LstELO, LstGid, Gid, ELO, Parts, Nurr, Narts ),
               atomic_list_concat( [Nurr|Narts], ':', Nexts )
     ).

chess_db_posi_value_kvx_update_best_op( <, NoCurr, Bim, LstELO, LstGid, Gid, ELO, Parts, Nurr, Narts ) :-
     ( NoCurr < Bim -> 
          Nurr is NoCurr + 1, 
          chess_db_posi_value_kvx_update_best_in( Parts, Gid, ELO, Marts ),
          Narts = [LstELO,LstGid|Marts]
          ;
          % fixme if NoCurr > Lim is an error
          Nurr is NoCurr,
          chess_db_posi_value_kvx_update_best_in( Parts, Gid, ELO, Narts )
     ).
chess_db_posi_value_kvx_update_best_op( =, NoCurr, Bim, LstELO, LstGid, Gid, ELO, Parts, Nurr, Narts ) :-
     ( NoCurr < Bim -> 
          Nurr is NoCurr + 1, 
          Narts = [ELO,Gid,LstELO,LstGid|Parts]
          ;
          Nurr is NoCurr,
          Narts = [LstELO,LstGid|Parts]
     ).
% identical to above
chess_db_posi_value_kvx_update_best_op( >, NoCurr, Bim, LstELO, LstGid, Gid, ELO, Parts, Nurr, Narts ) :-
     ( NoCurr < Bim -> 
          Nurr is NoCurr + 1, 
          Narts = [ELO,Gid,LstELO,LstGid|Parts]
          ;
          Nurr is NoCurr,
          Narts = [LstELO,LstGid|Parts]
     ).

chess_db_posi_value_kvx_update_best_in( [], Gid, ELO, Best ) :-
     Best = [ELO,Gid].
chess_db_posi_value_kvx_update_best_in( [LstELOAtm,LstGid|Parts], Gid, ELO, [AnELO,AnGid|Tarts] ) :-
     atom_number( LstELOAtm, LstELO ),
     ( LstELO < ELO -> 
          AnELO is LstELO, AnGid = LstGid,
          Carts = Parts, Zarts = Tarts
          ;
          AnELO is ELO, AnGid = Gid,
          Carts = [], Tarts = [LstELO,LstGid|Parts]  % ignoring Zarts
     ),
     chess_db_posi_value_kvx_update_best_in( Carts, Gid, ELO, Zarts ).

chess_db_posi_value_create( kvx, _Bim, Gid, ELO, Rex, Mv, Next ) :-
     chess_db_inc_res_index( Rex, res('0','0','0','0'), res(WN,DN,BN,UN) ),
     atomic_list_concat( [Mv,WN,DN,BN,UN], ':', NextMvs ),
     atomic_list_concat( [1,ELO,Gid], ':', NextEgi ),
     atomic_list_concat( [NextMvs,NextEgi], ';', Next ).

chess_db_inc_res_index( 1, res(Ws,Ds,Bs,Us), Res ) :-
     atom_number( Ws, WsN ),
     NxWs is WsN + 1,
     Res = res(NxWs,Ds,Bs,Us).
chess_db_inc_res_index( 2, res(Ws,Ds,Bs,Us), Res ) :-
     atom_number( Ds, DsN ),
     NxDs is DsN + 1,
     Res = res(Ws,NxDs,Bs,Us).
chess_db_inc_res_index( 3, res(Ws,Ds,Bs,Us), Res ) :-
     atom_number( Bs, BsN ),
     NxBs is BsN + 1,
     Res = res(Ws,Ds,NxBs,Us).
chess_db_inc_res_index( 4, res(Ws,Ds,Bs,Us), Res ) :-
     atom_number( Us, UsN ),
     NxUs is UsN + 1,
     Res = res(Ws,Ds,Bs,NxUs).

chess_db_res_index( '1-0', 1 ) :- !.
chess_db_res_index( '1/2-1/2', 2 ) :- !.
chess_db_res_index( '0-1', 3 ) :- !.
chess_db_res_index( '*', 4 ) :- !.
chess_db_res_index( Res, _ ) :- throw( cannot_convert_to_res_index(Res) ).

/** chess_db_limos_game_posi_obsolete( +Limos, +Gid, +Db ).

     Add a number of positions from Limos structures from game Gid, on to position table with db handle PoHa.
     
     Each position points to a string of Gid-Ply-Mv trips separated by ';'

     25.12.09 parking this as we plan to split this to two tables, 
     one keeping stats of next moves, (rather the refs to the gaems as 
*/
chess_db_limos_game_posi_obsolete( [], _Gid, _PosDb ).
chess_db_limos_game_posi_obsolete( [limo(Ply,_Hmv,Mv,Inpo)|T], Gid, PosDb ) :-
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
     chess_db_limos_game_posi_obsolete( T, Gid, PosDb ).

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
