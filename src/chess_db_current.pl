/**  chess_db_current( -CdbHs ).
     chess_db_current( -CdbHs, -Db ).

Returns the handles structure and Db location, CdbHs, for each open database.<br>

==
?- debug( chess_db(original) ).
?- chess_db( pack('chess_db/data/pgn/4ncl_short.pgn'), fourNCL.pgn, [dir('/tmp'),create(true)] ).
.....
?- chess_db_connect( '/tmp/fourNCL', profile(false) ).
true.
?- chess_db_current( Handles ).
Handles = chdbs(<#40f8e51c617f0000>, <#40f8e51c617f0000>, <#40f8e51c617f0000>).

?- chess_db_current( Handles, Db ).
Handles = chdbs(<#40f8e51c617f0000>, <#40f8e51c617f0000>, <#40f8e51c617f0000>),
Db = '/tmp/fourNCL'.

?- chess_db_current( CdbHs ), chess_db_max_id( CdbHs, Max ).
CdbHs = chdbs(<#40f8e51c617f0000>, <#40f8e51c617f0000>, <#40f8e51c617f0000>),
Max = 31.
==

@author   nicos angelopoulos
@version  0.1 2018/3/15
@version  0.2 2018/8/18, simplified to just an interface to chess_db_handles/2
@see      chess_db_connect/1

*/
chess_db_current( CdbHs ) :-
    chess_db_handles( _Db, CdbHs ).
chess_db_current( CdbHs, Db ) :-
    chess_db_handles( Db, CdbHs ).
