/**  chess_db_handles( +Create, +Pos, +Dir, -CdbHs, -AbsLoc )

Associate db handles CdbHs with chess db tables within Dir.<br>
Enables db creation if Create is true. Enables position table, if Pos is true.
Returns the absolute location of the Db in AbsLoc.

==
?- chess_db_handles( true, true, '/tmp/ex_db', Cdbhs ).
==

@author nicos angelopoulos
@version  0.1 2018/3/17
@version  0.1 2018/8/16, added AbsLoc

*/
chess_db_handles( Create, Pos, Dir, CdbHs, AbsDir ) :-
    chess_db_dir( Dir, Create, AbsDir ),
    debug( chess_db(db), 'Abs dir: ~p', [AbsDir] ),
    ( Pos == true -> PosL = [posi]; PosL = [] ),
    chess_db_connect_subs( [info,move,orig|PosL], AbsDir, Create, CdbHsL ),
    CdbHs =.. [chdbs|CdbHsL].

chess_db_connect_subs( [], _Dir, _Create, [] ).
chess_db_connect_subs( [Sub|Subs], Dir, Create, [SubHa|Has] ) :-
    chess_db_connect_sub( Dir, Create, Sub, SubHa ),
    chess_db_connect_subs( Subs, Dir, Create, Has ).

chess_db_handle( Table, CdbHs, Handle ) :-
     ( chess_db_handle_arg(Table, CdbHs, Handle) ->
          true
          ;
          % fixme: add caller trail...
          throw( no_handle(Table,CdbHs), [pack(chess_db),pred(chess_db_handle/3)] )
     ).

chess_db_handle_arg( info, CdbHs, InfoHandle ) :-
     arg( 1, CdbHs, InfoHandle ).
chess_db_handle_arg( move, CdbHs, MoveHandle ) :-
     arg( 2, CdbHs, MoveHandle ).
chess_db_handle_arg( orig, CdbHs, MoveHandle ) :-
     arg( 3, CdbHs, MoveHandle ).
chess_db_handle_arg( posi, CdbHs, PosiHandle ) :-
     arg( 4, CdbHs, PosiHandle ).

chess_db_handles_close( CdbHs ) :-
     arg( _, CdbHs, Handle ),
     db_disconnect( Handle ),
     fail.
chess_db_handles_close( _CdbHs ).

chess_db_connect_sub( Dir, Create, Db, Handle ) :-
     atomic_list_concat( [game,Db], '_', Base ),
     file_name_extension( Base, sqlite, SqliteF ),
     directory_file_path( Dir, SqliteF, DbF ),
     chess_db_connect_to( DbF, Create, Base, Handle ).

% fixme: see chess_db_connect_dir
chess_db_connect_to( DbF, _Create, _Base, Handle ) :-
     ( exists_file(DbF) ; exists_directory(DbF) ),
     !,
     chess_db_connect_handle( DbF, Handle ).
chess_db_connect_to( DbF, Create, Base, Handle ) :-
     chess_db_connect_to_create( Create, DbF, Base, Handle ).

chess_db_connect_to_create( false, DbF, _Base, _Handle ) :-
     debug( chess_db(info), 'Not creating chess_db file: ~p', DbF ),
     !,
     fail.
chess_db_connect_to_create( true, DbF, Base, Handle ) :-
     chess_db_create( DbF, Base, Handle ).

