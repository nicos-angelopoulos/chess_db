/**  chess_db_list( +Dir ).

A simple, on-screen dump of all data in a chess games database.<br>
Likely to be only useful for debugging.

==
?- chess_db_list( short ).
==

@author nicos angelopoulos
@version  0.1 2018/3/14

*/
chess_db_list( Db ) :-
    chess_db_connect( Db, db(DbO) ),
    % chess_db_handles( false, false, Db, CdbHs, _AbsDb ),
    chess_db_current( CdbHs, DbO ),
    chess_db_max_id( CdbHs, Max ),
    chess_db_handle( info, CdbHs, InfHa ),
    chess_db_handle( move, CdbHs, MovHa ),
    chess_db_list( Max, 1, InfHa, MovHa ),
    chess_db_disconnect( DbO ).

chess_db_list( Max, I, _InfHa, _MovHa ) :-
    I > Max, 
    !.
chess_db_list( Max, I, InfHa, MovHa ) :-
    write( I ), write( ': ' ), nl,
    findall( _, (   db_holds(InfHa,game_info(I,K,V)),
                    write(K=V),write(';')
                ), _ ),
    nl,
    findall( _, (   db_holds(MovHa,game_move(I,MvNo,Turn,Move)),
                    write(MvNo+Turn+Move),write(';')
                ), _ ),
    nl,

    J is I + 1,
    chess_db_list( Max, J, InfHa, MovHa ).
