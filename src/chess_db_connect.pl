
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:select_all/4).
:- lib(chess_db_exists/1).

chess_db_connect_defaults( [profile(true),create(false),position(false)] ).

/** chess_db_connect( +DbS ).
    chess_db_connect( +DbS, +Opts ).

Connect a number of chess_dbs (DbS) as open db handles that will <br>
be available to a number of predicates that access the information,<br>
for example chess_db_openning/3.

Opts
  * create(Create=false)
     if true create dir and/or db files if they do not exist
  * dir(Dir=_)
     parent directory of chess database (mutliple are allowed)
  * profile(Prof=Prof)
     whether to mix, true, or ignore, false, profile based dir options (see options_append/4)
     if no dir(Dir) option is present in Opts, then Prof is ignored

Note, that standard database directories can be defined long term in ~/.pl/chess_db_connect.pl

==
% cat ~/.pl/chess_db_connect.pl
dir( '/usr/local/users/chess/chess_db' ).
?- chess_db_connect( dir('/tmp/') ).
ERROR: Unhandled exception: chess_db_dir_does_not_exist_and_asked_not_to_create_it_by('/usr/local/users/nicos/sware/nicos/git/github/stoics.infra/here',false)

?- chess_db_connect( true ).
HERE: 
false.
==

@author nicos angelopoulos
@version  0.1 2018/3/15
@see chess_db_openning/3

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
    chess_db_connect_dirs( Dbs, Dirs, Create, Pos ).

chess_db_connect_dirs( [], _Dirs, _Create, _Pos ).
chess_db_connect_dirs( [Db|Dbs], Dirs, Create, Pos ) :-
    chess_db_connect_db( Create, Pos, Db, Dirs ),
    chess_db_connect_dirs( Dbs, Dirs, Create, Pos ).

chess_db_connect_db( false, Pos, Db, Dirs ) :-
    member( Dir, [''|Dirs] ),
    Apts = [relative_to(Dir),solutions(all),file_type(directory)], % fixme non dir dbs...
    absolute_file_name( Db, Abs, Apts ),
    chess_db_exists( Abs ),
    chess_db_connect_abs_dir( Abs, false, Pos ),
    !.
chess_db_connect_db( true, Pos, Db, Dirs ) :-
    member( Dir, [''|Dirs] ),
    Apts = [relative_to(Dir),solutions(all),file_type(directory)], % fixme non dir dbs...
    absolute_file_name( Db, Abs, Apts ),  % fixme any options ?
    chess_db_connect_abs_dir( Abs, true, Pos ),
    !.
chess_db_connect_dir( _Create, _Pos, Dir ) :-
    debug( chess_db(true), 'Failed to connect to chess_db at dir: ~p', [Dir] ).

chess_db_connect_abs_dir( Abs, _Create, _Pos ) :-
    chess_db_handles( Abs, _ ),
    !,
    debug( chess_db, 'Handles already exist, for chess_db directory: ~p', [Abs] ).
chess_db_connect_abs_dir( Abs, Create, Pos ) :-
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
