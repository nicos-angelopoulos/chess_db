/**  chess_db_game_id( -GameID ).

Return all the unique game ids, constructed as Handles:Gno.<br>
Handles are the handles to access an open chess id and Gno is the<br>
unique game id for a game in that database.

==
?- chess_db_connect( [dir('/usr/local/users/chess/chess_db/18.07-Biel'),profile(false),position(true)] ).

?- chess_db_game_id(Gid).
Gid = chdbs(<#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>):1 ;
Gid = chdbs(<#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>):2 ;
==

@author nicos angelopoulos
@version  0.1 2018/8/15

*/
chess_db_game_id( Gid ) :-
    chess_db_current( CdbHs ),
    chess_db_handle( info, CdbHs, InfoH ),
    % setof( AGno, (K,V)^db_holds(InfoH,game_info(AGno,K,V)), Gnos ),
    findall( AGno, db_holds(InfoH,game_info(AGno,_,_)), AllGnos ),
    sort( AllGnos, Gnos ),
    member( Gno, Gnos ),
    Gid = CdbHs:Gno.
