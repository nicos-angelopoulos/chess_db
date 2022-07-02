
chess_db_defaults( [create(false),position(true)] ).

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

Opts
  * create(Create=false)
     how to behave if ChessDb exists (see chess_db_connect/2)

  * db(OptDb)
     database location (see chess_db_connect/2)

  * dir(Dir)
     directory where database is located, (many allowed, see chess_db_connect/2)

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
    pgn( PgnIn, Pgn ),
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
    chess_db_games_add( Pgn, LaGid, CdbHs ),
    ( atomic(AbsDb) -> chess_db_disconnect(AbsDb); true ),
    % chess_db_handles_close( CdbHs ),
    ( var(ArgDb) -> ArgDb = AbsDb; true ),
    ( var(OptDb) -> OptDb = AbsDb; true ).

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
