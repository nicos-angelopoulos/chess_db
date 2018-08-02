
chess_db_defaults( [create(false),position(false)] ).

/** chess_db( +PgnOrF ).
    chess_db( +PngOrF, +Opts ).

Add games from a PGN file or a PGN term, PgnOrF, to the chess database 
pointed to by Opts.

Opts
  * create(Create=false)
     if true create dir and/or db files if they do not exist
  * dir(Dir=_)
     directory for database
  * position(Pos=false)
     if true, use position table

Options can also be picked up from ~/.pl/chess_db.pl (see options_append/3).

==
?- working_directory( Old, pack('chess_db/examples') ), assert( push_dir(Old) ).
?- Opts = [dir('/tmp/chess_dbase'),create(true)],
   chess_db( '18.03.10-Hampstead.pgn', Opts ).
==

@author nicos angelopoulos
@version  0.1 2018/3/14
@see options_append/3

*/
chess_db( PgnIn ) :-
    chess_db( PgnIn, [] ).

chess_db( PgnIn, Args ) :-
    options_append( chess_db, Args, Opts ),
    options( dir(Dir), Opts ),
    options( create(Create), Opts ),
    options( position(Pos), Opts ),
    pgn( PgnIn, Pgn ),
    chess_db_handles( Create, Pos, Dir, CdbHs ),
    chess_db_handle( info, CdbHs, InfoHandle ),
    chess_db_max_id( InfoHandle, LaGid ),
    chess_db_games_add( Pgn, LaGid, CdbHs),
    chess_db_handles_close( CdbHs ).

chess_db_games_add( [], _Gid, _CdbHs ).
chess_db_games_add( [G|Gs], Gid, CdbHs ) :-
    % fixme: ignore position for now
    G = pgn(Info,Moves,_Res,Orig),
    chess_db_handle( info, CdbHs, InfoHandle ),
    chess_db_handle( move, CdbHs, MoveHandle ),
    chess_db_handle( orig, CdbHs, OrigHandle ),
    chess_db_game_add( InfoHandle, Info, Moves, Orig, Gid, MoveHandle, OrigHandle, Nid ),
    chess_db_games_add( Gs, Nid, CdbHs ).

chess_db_game_add( InfoHandle, Info, _Moves, _Orig, Gid, _MoHa, _OrHa, Gid ) :-
    chess_db_game_info_exists( Info, InfoHandle, ExGid ),
    !, % fixme: add option for erroring
    debug( chess_db, 'Info match existing game: ~d', ExGid ).
chess_db_game_add( InfoHandle, Info, Moves, Orig, Gid, MoHa, OrHa, Nid ) :-
    Nid is Gid + 1,
    findall( game_info(Nid,K,V), member(K-V,Info), Goals ),
    db_assert( InfoHandle, Goals, _ ),
    findall( game_move(Nid,N,Turn,Move), (
                                            member( move(N,Mv1,Mv2,_Cmm1,_Cms2),Moves),
                                            nth1( Idx, [Mv1,Mv2], Move ),
                                            Move \== [],   % Mv2 really
                                            nth1( Idx, [false,true], Turn )  % check values are db asserted properly
                                         ), Moals ),
    db_assert( MoHa, Moals, _ ),
    maplist( atom_codes, OrigAtms, Orig ),
    atomic_list_concat( OrigAtms, '\n', OrigAtm ),
    debug( chess_db(original), '~a', OrigAtm ),
    db_assert( OrHa, game_orig(Nid,OrigAtm), _ ).

chess_db_game_info_exists( [], _InfHa, _ExGid ).
chess_db_game_info_exists( [K-V|T], InfHa, ExGid ) :-
    db_holds( InfHa, game_info(ExGid,K,V) ),
    chess_db_game_info_exists( T, InfHa, ExGid ).

chess_db_table_fields( game_info, [gid+integer,key+text,value-text] ).
chess_db_table_fields( game_move, [gid+integer,move_no+integer,turn+boolean,move-text] ).
chess_db_table_fields( game_orig, [gid+integer,original-text] ).
chess_db_table_fields( game_position, [position+text,gid+integer] ).

chess_db_dir( Dir, _Create ) :-
    exists_directory( Dir ),
    !,
    % fixme: can the following into a debug_call(,dir,chess_db/Dir).
    debug( chess_db, 'Using existing chess_db directory: ~w', Dir ).
chess_db_dir( Dir, Create ) :-
    Create == true,
    !,
    debug( chess_db, 'Creating new chess_db directory: ~w', Dir ),
    make_directory_path( Dir ).
chess_db_dir( Dir, Create ) :-
    throw( chess_db_dir_does_not_exist_and_asked_not_to_create_it_by(Dir,Create) ).

