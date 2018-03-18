
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:select_all/4).

chess_db_connect_defaults( [profile(true),create(false),position(false)] ).

/** chess_db_connect( Opts ).

Connect a number of chess_dbs as open db handles that will <br>
be available to a number of predicates that access the information, <br>
for example chess_db_openning/3.

Opts
  * create(Create=false)
     if true create dir and/or db files if they do not exist
  * dir(Dir=_)
     directory of chess database (multiple are allowed, see chess_db_connect/1)
  * profile(Prof=Prof)
     whether to mix, true, or ignore, false, profile based dir options (see options_append/4)
     if no dir(Dir) option is present in Opts, then Prof is ignored

Note, that standard databases can be defined long term in ~/.pl/chess_db_connect.pl

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
chess_db_connect( ArgS ) :-
    en_list( ArgS, Args ),
    select_all( Args, dir(_D), Sel, Rem ),
    options_append( chess_db_connect, Rem, OptsPrv ),
    options( profile(Prof), OptsPrv ),
    chess_db_sel_connect_dirs( Sel, Prof, OptsPrv, Opts ),
    findall( Dir, member(dir(Dir),Opts), Dirs ),
    options( create(Create), Opts ),
    options( position(Pos), Opts ),
    chess_db_connect_dirs( Dirs, Create, Pos ).

chess_db_connect_dirs( [], _Create, _Pos ).
chess_db_connect_dirs( [Dir|Ds], Create,Pos ) :-
    chess_db_connect_dir( Create, Pos, Dir ),
    chess_db_connect_dirs( Ds, Create, Pos ).

chess_db_connect_dir( Create, Pos, Dir ) :-
    absolute_file_name( Dir, Abs ),  % fixme any options ?
    chess_db_connect_abs_dir( Abs, Create, Pos ).

chess_db_connect_abs_dir( Abs, _Create, _Pos ) :-
    chess_db_handles( Abs, _ ),
    !,
    debug( chess_db, 'Handles already exist, for chess_db directory: ~p', [Abs] ).
chess_db_connect_abs_dir( Abs, Create, Pos ) :-
    chess_db_handles( Create, Pos, Abs, CdbHs ),
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
