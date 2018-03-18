
chess_db_current_defaults( [close(_)] ).

/**  chess_db_current( -CdbHs ).
     chess_db_current( -CdbHs, +Opts ).

If there are open databases the handles structure of each is returned.<br>
Else, chess_db_connect(Opts) is called to create the connections.<br>

Opts
  * close(Close=_)
     if the databases were opened by this call, then Close will be instantiated to true

  * dir(Dir=_)
     directory of chess database (multiple are allowed, see chess_db_connect/1)

==
?- debug( chess_db(original) ).
?- chess_db( pack('chess_db/data/4ncl_short.pgn'), [dir('/tmp/fourNCL'),create(true)] ).
.....
?- chess_db_connect( [dir('/tmp/fourNCL'),profile(false)] ).
true.
?- chess_db_current( CdbHs ).
CdbHs = chdbs(<#40e8de28067f0000>, <#40e8de28067f0000>, <#40e8de28067f0000>).
?- chess_db_current( CdbHs ), chess_db_max_id( CdbHs, Max ).
Max = 31.
==

@author   nicos angelopoulos
@version    0.1 2018/3/15
@see      chess_db_connect/1

*/
chess_db_current( CdbHs ) :-
    chess_db_current( CdbHs, [] ).
chess_db_current( CdbHs, Args ) :-
    options_append( chess_db_current, Args, Opts ),
    chess_db_current_opts( CdbHs, Opts ).

chess_db_current_opts( CdbHs, Opts ) :-
    findall( ACdbHs, chess_db_handles(_,ACdbHs), CdbHsL ),
    CdbHsL \== [],
    memberchk( close(false), Opts ),
    !,
    member( CdbHs, CdbHsL ).
chess_db_current_opts( CdbHs, Opts ) :-
    chess_db_connect( Opts ),
    memberchk( close(true), Opts ),
    findall( CdbHs, chess_db_handles(_,CdbHs), CdbHsL ),
    member( CdbHs, CdbHsL ).
