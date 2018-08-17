/**  chess_db_id_info( +Gid, -Key, -Value ).

Get Key and Value for info db corresponding to the game id GID.<br>
Game id is of the form DBHandles:GameNo.

==
?- chess_db_connect( [dir('/usr/local/users/chess/chess_db/18.07-Biel'),profile(false),position(true)] ).
?- chess_db_game_id(Gid), chess_db_id_info(Gid,'Result',Result).
Gid = chdbs(<#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>):1,
Result = '1-0' ;
Gid = chdbs(<#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>):2,
Result = '1-0' ;
Gid = chdbs(<#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>):3,
Result = '1/2-1/2' ;
Gid = chdbs(<#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>, <#40380f90857f0000>):4,
Result = '0-1' 
...

?- % give me all Sicilian defense results with opponent names
?- chess_db_openning( [e4,c5], Gid ), chess_db_id_info( Gid, 'Result', Result ),
   chess_db_id_info( Gid, 'White', White ),
   chess_db_id_info( Gid, 'Black', Black ),
   write( Result:White/Black ), nl, fail.

1-0:Svidler, Peter/Georgiadis, Nico
1/2-1/2:Carlsen, Magnus/Svidler, Peter
0-1:Georgiadis, Nico/Mamedyarov, Shakhriyar
1/2-1/2:Carlsen, Magnus/Vachier-Lagrave, Maxime
0-1:Georgiadis, Nico/Svidler, Peter
1-0:Carlsen, Magnus/Georgiadis, Nico
(*):Carlsen, Magnus/Georgiadis, Nico
==

@author nicos angelopoulos
@version  0.1 2018/8/15

*/
chess_db_id_info( CdbHs:Gno, Key, Val ) :-
    ground( CdbHs ), % fixme: allow backtrackable ? it doesn't seem so useful
    chess_db_handle( info, CdbHs, InfoHandle ),
    db_holds( InfoHandle, game_info(Gno,Key,Val) ).
