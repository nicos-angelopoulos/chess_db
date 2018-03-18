/**  chess_db_disconnect( ?Db )

Disconnect from a Db or all Dbs (-Db).

Db can be 
  * a directory of an open chess_db/0
  * a CdbHs chess_db streams structure
  * a open stream from within a CdbHs (all dbs are closed)

==
?- debug( chess_db(original) ).
?- chess_db( pack('chess_db/examples/4ncl_short.pgn'), [dir('/tmp/fourNCL'),create(true)] ).
...
?- chess_db_connect( [dir('/tmp/fourNCL'),profile(false)] ).
true.
?- chess_db_current( CdbHs .
CdbHs = chdbs(<#40e867375c7f0000>, <#40e867375c7f0000>, <#40e867375c7f0000>).
?- chess_db_disconnect( CdbHs ).
==

@author nicos angelopoulos
@version  0.1 2018/3/18

*/
chess_db_disconnect( Db ) :-
    var( Db ),
    !,
    chess_db_disconnect_var( Db ).
chess_db_disconnect( Db ) :-
    atomic( Db ),
    chess_db_disconnect_atomic( Db ),
    !.
chess_db_disconnect( Db ) :-
    compound( Db ),
    chess_db_handles( Dir, CdbHs ),
    !,
    chess_db_disconnect_handles( CdbHs ),
    retract( chess_db_handles(Dir,CdbHs) ).
chess_db_disconnect( Other ) :-
    throw( do_not_know_how_to_disconnect_from(Other) ).

chess_db_disconnect_var( _Db ) :-
    chess_db_handles( Dir, CdbHs ),
    chess_db_disconnect_handles( CdbHs ),
    retract( chess_db_handles(Dir,CdbHs) ).

chess_db_disconnect_atomic( Dir ) :-
    exists_direcdtory( Dir ),
    chess_db_handles( Dir, CdbHs ),
    !,
    chess_db_disconnect_handles( CdbHs ),
    retract( chess_db_handles(Dir,CdbHs) ).
chess_db_disconnect_atomic( Stream ) :-
    is_stream( Stream ),
    chess_db_handles( Dir, CdbHs ),
    arg( _, CdbHs, Stream ),
    !,
    chess_db_disconnect_handles( CdbHs ),
    retract( chess_db_handles(Dir,CdbHs) ).

chess_db_disconnect_handles( CdbHs ) :-
    functor( CdbHs, _Name, Arity ),
    findall( _, ( between(1,Arity,I),arg(I,CdbHs,Stream),
                  close( Stream )
                ), _ ).
