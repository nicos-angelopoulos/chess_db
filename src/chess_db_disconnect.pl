/**  chess_db_disconnect.
     chess_db_disconnect( ?DbS )

Disconnect from a number of chess dbs. 
Can disconnect from all Dbs either one at a time when -Db using backtracking,
or in a single deterministic call with the /0 version.

Db can be 
  * a directory of an open chess_db/0
  * a CdbHs chess_db streams structure
  * an open handle from within a CdbHs (all dbs are closed)
  * a list of any of the above

==
?- debug( chess_db(original) ).
?- chess_db( pack('chess_db/data/pgn/4ncl_short.pgn'), fourNCL, [dir('/tmp'),create(true),db(Db)] ).
...
Db = '/tmp/fourNCL'.
?- chess_db_connect( fourNCL, [dir('/tmp'),profile(false)] ).
true.
?- chess_db_current( CdbHs ).
CdbHs = chdbs(<#40e867375c7f0000>, <#40e867375c7f0000>, <#40e867375c7f0000>).
?- chess_db_current( CdbHs ), chess_db_disconnect( CdbHs ).
==

@author nicos angelopoulos
@version  0.1 2018/3/18

*/
chess_db_disconnect :-
    findall( _, chess_db_disconnect(_), _ ).

chess_db_disconnect( Db ) :-
    var( Db ),
    !,
    chess_db_disconnect_var( Db ).
chess_db_disconnect( [] ) :- !.
chess_db_disconnect( [H|T] ) :-
    !,
    chess_db_disconnect( H ),
    chess_db_disconnect( T ).
chess_db_disconnect( Db ) :-
    atomic( Db ),
    chess_db_disconnect_atomic( Db ),
    !.
chess_db_disconnect( CdbHs ) :-
    compound( CdbHs ),
    chess_db_handles( Db, CdbHs ),
    !,
    chess_db_disconnect_handles( Db, CdbHs ).
chess_db_disconnect( Other ) :-
    throw( do_not_know_how_to_disconnect_from(Other) ).

chess_db_disconnect_var( Db ) :-
    chess_db_handles( Db, CdbHs ),
    chess_db_disconnect_handles( Db, CdbHs ).

chess_db_disconnect_atomic( Db ) :-
    exists_directory( Db ),
    chess_db_handles( Db, CdbHs ),
    !,
    chess_db_disconnect_handles( Db, CdbHs ).
chess_db_disconnect_atomic( Stream ) :-
    is_stream( Stream ),
    chess_db_handles( Db, CdbHs ),
    arg( _, CdbHs, Stream ),
    !,
    chess_db_disconnect_handles( Db, CdbHs ).

chess_db_disconnect_handles( Db, CdbHs ) :-
    functor( CdbHs, _Name, Arity ),
    findall( _, ( between(1,Arity,I),arg(I,CdbHs,Stream),
                  db_disconnect( Stream )
                ), _ ),
    debug( chess_db, 'Closing chess db: ~p', Db ),
    retract( chess_db_handles(Db,CdbHs) ).
