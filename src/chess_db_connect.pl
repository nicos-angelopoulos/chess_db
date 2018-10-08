
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:select_all/4).
:- lib(chess_db_exists/1).

chess_db_connect_defaults( Defs ) :-
    Defs = [profile(true), create(false), position(false), db(_), handles(_)].

/** chess_db_connect( +DbS ).
    chess_db_connect( +DbS, +Opts ).

Connect to a number of chess_dbs (DbS) and provide make their db handles available<br>
to a number of predicates that access the information: eg chess_db_opening/3.<br>
The library provides two conveniencies for locating chess dbs. First, via aliases:<br>
chess_db (by default expands to pack(chess_db_data/dbs)). Second, via dir(Dir) option.<br>
In this case Dbs are looked for relative to all Dir locations provided.<br>
Note that commonly used database directories can be defined long term in 
~/.pl/chess_db_connect.pl<br>
(see options_append/4).

Opts
  * create(Create=false)
     if true create dir and/or db files if they do not exist
     if =|true|= create ChessDb if doesn't exist (and use current if it does)
     if =|false|= only proceed if ChessDb exists
     if =|new|= only proceed if ChessDb does not exist (call creates it)
     if =|fresh|= overwrites if a current exists

  * db(Db)
     returns the absolute locations of the dbs successfully connected (a list iff more than one)

  * dir(Dir)
     parent directory of chess database (mutliple are allowed)

  * handles(Handles)
     returns the handles term of connected databases (a list if multiple were established)

  * profile(Prof=Prof)
     whether to mix, true, or ignore, false, profile based dir options (see options_append/4)
     if no dir(Dir) option is present in Opts, then Prof is ignored

==
% connect with alias
?- Db = chess_db('18.03-candidates'),
   ( chess_db_connect(Db, db(AbsDb) ) -> true
      ; chess_db( pgn('18.03-candidates'), Db, create(true) ),
        chess_db_connect( Db, db(AbsDb) )
   ).
Db = chess_db('18.03-candidates'),
AbsDb = ['/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/chess_db_data/dbs/18.03-candidates'].

?- chess_db_disconnect( Db ).
Db = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/chess_db_data/dbs/18.03-candidates'.

% connect with dir option in profile options...

?- shell( 'cat ~/.pl/chess_db_connect.pl' ).
dir( '/usr/local/users/chess/chess_db' ).
dir( '/usr/local/users/nicos/local/git/lib/swipl/pack/chess_db_data/dbs' ).
true.

?- read_link( '/usr/local/users/nicos/local/git/lib/swipl', A, B ).
A = 'swipl-7.7.18/',
B = '/usr/local/users/nicos/local/git/lib/swipl-7.7.18/'.

?- chess_db_connect('18.03-candidates', db(Db) ).
Db = ['/usr/local/users/nicos/local/git/lib/swipl-7.7.18/pack/chess_db_data/dbs/18.03-candidates'].
==

@author nicos angelopoulos
@version  0.1 2018/3/15
@version  0.2 2018/8/17, added aliases, better locator (and order), fixes, docs/examples
@see chess_db_opening/3

*/
chess_db_connect( DbS ) :-
    chess_db_connect( DbS, [] ).

chess_db_connect( DbS, ArgS ) :-
    en_list( DbS, Dbs ),
    en_list( ArgS, Args ),
    select_all( Args, dir(_D), Sel, Rem ),
    options_append( chess_db_connect, Rem, OptsPrv ),
    options( profile(Prof), OptsPrv ),
    chess_db_sel_connect_dirs( Sel, Prof, OptsPrv, Opts ),
    findall( Dir, member(dir(Dir),Opts), Dirs ),
    options( create(Create), Opts ),
    options( position(Pos), Opts ),
    chess_db_connect_dirs( Dbs, Dirs, Create, Pos, CdbHssPrv, AbssPrv ),
    de_list( AbssPrv, Abss ),
    de_list( CdbHssPrv, CdbHss ),
    memberchk( db(Abss), Opts ),
    memberchk( handles(CdbHss), Opts ).

de_list( List, Elem ) :- % move to pack(stoics_lib)
    is_list( List ),
    List = [Elem],
    !.
de_list( Other, Other ).

chess_db_connect_dirs( [], _Dirs, _Create, _Pos, [], [] ).
chess_db_connect_dirs( [Db|Dbs], Dirs, Create, Pos, CdbHss, Abss ) :-
    chess_db_connect_db( Create, Pos, Db, Dirs, CdbHs, Abs ),
    ( Abs == [] -> Abss = TAbss ; Abss = [Abs|TAbss] ),
    ( CdbHs == [] -> CdbHss = TCdbHss ; CdbHss = [CdbHs|TCdbHss] ),
    chess_db_connect_dirs( Dbs, Dirs, Create, Pos, TCdbHss, TAbss ).

chess_db_connect_db( false, Pos, Db, Dirs, CdbHs, AbsConn ) :-
    % append( Dirs, [''], Airs ),
    member( Dir, [''|Dirs] ),
    Apts = [relative_to(Dir),solutions(all),file_type(directory)], % fixme non dir dbs...
    absolute_file_name( Db, Abs, Apts ),
    chess_db_exists( Abs ),
    chess_db_connect_abs_dir( Abs, false, Pos, CdbHs, AbsConn ),
    !.
chess_db_connect_db( new, Pos, Db, Dirs, CdbHs, AbsConn ) :-
    append( Dirs, [''], Airs ),
    member( Dir, Airs ),
    Apts = [relative_to(Dir),solutions(all)], % fixme non dir dbs...
    absolute_file_name( Db, Abs, Apts ),
    chess_db_connect_abs_dir( Abs, true, Pos, CdbHs, AbsConn ),
    !.
chess_db_connect_db( true, Pos, Db, Dirs, CdbHs, AbsConn ) :-
    ( chess_db_connect_db( false, Pos, Db, Dirs, CdbHs, AbsConn )  ->
        true
        ;
        chess_db_connect_db( new, Pos, Db, Dirs, CdbHs, AbsConn )
    ).
chess_db_connect_db( fresh, Pos, Db, Dirs, CdbHs, AbsConn ) :-
    member( Dir, [''|Dirs] ),
    Apts = [relative_to(Dir),solutions(all),file_type(directory)], % fixme non dir dbs...
    absolute_file_name( Db, Abs, Apts ),
    chess_db_exists( Abs ),
    !,
    debug( chess_db(true), 'Deleting chess_db at: ~p', [Abs] ),
    delete_directory_and_contents( Abs ),
    chess_db_connect_abs_dir( Abs, true, Pos, CdbHs, AbsConn ).

chess_db_connect_db( _Create, _Pos, Dir, [], [] ) :-
    debug( chess_db(true), 'Failed to connect to chess_db at dir: ~p', [Dir] ).

chess_db_connect_abs_dir( Abs, _Create, _Pos, CdbHs, [] ) :-
    chess_db_handles( Abs, CdbHs ),
    !,
    debug( chess_db(info), 'Handles already exist, for chess_db directory: ~p', [Abs] ).
chess_db_connect_abs_dir( Abs, Create, Pos, CdbHs, Abs ) :-
    chess_db_handles( Create, Pos, Abs, CdbHs, _AbsDir ),
    !,
    assertz( chess_db_handles(Abs,CdbHs) ).
    
chess_db_sel_connect_dirs( [], _Prof, OptsPrv, Opts ) :-
    Opts = OptsPrv.
chess_db_sel_connect_dirs( [H|T], Prof, OptsPrv, Opts ) :-
    ( Prof == false ->
        select_all( OptsPrv, dir(_D), _, OptsNonD ),
        append( [H|T], OptsNonD, Opts )
        ;
        append( [H|T], OptsPrv, Opts )
    ).
