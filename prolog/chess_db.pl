:- module( chess_db, [ 
                        % io.(pgn + dbs)
                        pgn/2,                                  % +PgnToF, ?PgnT
                        chess_db/1,chess_db/2,chess_db/3,       % +PgnToF[, Db[, +Opts]]
                        chess_db_ids_pgn/2,                     % +GidS, +PgnF
                        % db.connections
                        chess_db_disconnect/0,                  %
                        chess_db_disconnect/1,                  % ?Db
                        chess_db_connect/1,chess_db_connect/2,  % +Dbs[, +Opts]
                        chess_db_current/1, chess_db_current/2, % -CdbHs[,-Db]
                        % db.games
                        chess_db_game/1,                        % -GameID
                        chess_db_game_info/3,                   % ?Gid, -Key, -Val
                        chess_db_match/2,                       % +Patts, ?GameS
                        chess_db_opening/2,                    % +Moves, ?GameS
                        chess_db_opening_pgn/2,                % +Moves, +PgnF
                        % db.info
                        chess_db_list/1,                        % +Dir
                        chess_db_max_id/2,                      % +HandleST, -Max
                        % chess.dict
                        chess_dict_start_board/1 ,              % -Brd
                        chess_dict_inpo/2,                      % ?Brd, ?Inpo
                        chess_pgn_limos/2,                      % +PgnTerm, -Limos
                        chess_dict_move/3,chess_dict_move/4,    % +Move, +DictI, [+Turn,] -DictO
                        chess_annotate_freq/3,                  % +Pgn, +CheDbLoc, +Args
                        chess_fen_square/2,                     % ?Fen, ?Sqr
                        chess_algebraic_square/2,               % ?Alg, ?Sqr
                        % etc
                        chess_db_version/2                      % -Vers, -Date
                     ] ).

chess_db_alias( Alias, Path ) :-
    user:file_search_path( Alias, Path ),
    !.
chess_db_alias( Alias, Path ) :-
    assertz( user:file_search_path(Alias,Path) ).

:- multifile(user:file_search_path/2).
:- chess_db_alias( pgn, pack('chess_db/data/pgn') ).
:- chess_db_alias( chess_db, pack('chess_db/data/dbs') ).

% user:file_search_path( pgn, pack('chess_db/data/pgn') ).
% user:file_search_path( pgn, pack('chess_db_data/pgn') ).
% user:file_search_path( chess_db, pack('chess_db_data/dbs') ).

:- use_module(library(lib)).

:- lib(options).
:- lib(os_lib).
:- lib(db_facts).
:- lib(prosqlite).
:- lib(stoics_lib).
:- lib(debug_call).

:- dynamic(chess_db_handles/2).       % +Dir, +HandlesTerm

:- lib(source(chess_db),homonyms(true)).

:- lib(pgn/2).
:- lib(chess_db/2).
:- lib(chess_db_list/1).
:- lib(chess_db_max_id/2).
:- lib(chess_db_game/1).
:- lib(chess_db_ids_pgn/2).
:- lib(chess_db_connect/1).
:- lib(chess_db_current/2).
:- lib(chess_db_game_info/3).
:- lib(chess_db_match/2).
:- lib(chess_db_opening/2).
:- lib(chess_db_disconnect/1).
:- lib(chess_db_opening_pgn/2).
:- lib(chess_db_handles/4).
:- lib(chess_db_messages/0).
:- lib(chess_dict/0).
:- lib(chess_fen_square/2).
:- lib(chess_algebraic_square/2).
:- lib(chess_dict_move/4).
:- lib(chess_pgn_limos/2).
:- lib(chess_annotate_freq/3).

:- debuc(chess_db(true)).
:- lib(end(chess_db)).

/**  <module> PGN and chess game databases.

This library produces chess games databases from PGN files and provides some<br>
predicates for manipulating these databases.

Once connected to a number of chess_db databases, information about the games <br>
can be interrogated. (See chess_db_opening/2 for an example.)

Ideally we want to hook this pack to either a web-based interface, or (b) have an engive interface to exploit GUI playing programs, for playing the games as we select them.<br>
Currently selected games can be saved to a PGN file and be displayed with any PGN displaying program.

---+++ Installation: 

==
?- pack_install( chess_db ).
==

The pack includes code to:
  * parse PGN's 
     pgn/2
  * store the parsed terms into prosqlite databases
     chess_db/2
  * manipulate the resulting databases
     chess_db_opening_pgn/2

There are three example pgns in pack(chess_db/data/pgn) and an example program, examples/short.pl

---+++ Dependencies

Packs:
  * prosqlite (1.5)
  * db_facts  (0.5)
  * stoics_lib (1.0)
  * options (1.0)
  * debug_call (1.1)

---+++ Basics

By default, chess database dirs contain 3 sqlite databases:
  * game_info.sqlite contains game_info(Gid,Key,Val) info about each game
  * game_move.sqlite table is of form game_move(Gid,Num,Turn,Move)
  * game_orig.sqlite table is game_orig(Gid,Orig); where Orig is the verbatim of the section in the PGN for that game

A number of databases can be connected at the same time. Operations
are implicit to all open databases. Connecting is via chess_db_connect/2.

---+++ Example

==
?- [pack('chess_db/examples/short.pl')].
?- short.      % creates a chess_db in subdirectory short/ from data/4ncl_short.pgn
               % and displays the game ids for games that start with [e4,e6] (French defence)
...

?- french.     % creates a new pgn file from the base for the 2 games in short/ that start with e4,e6
% Using existing chess_db directory: /home/nicos/short
% (short) Following games start with 1.e4,e6
gid(chdbs(<#4078544a8f7f0000>,<#4078544a8f7f0000>,<#4078544a8f7f0000>):1)
gid(chdbs(<#4078544a8f7f0000>,<#4078544a8f7f0000>,<#4078544a8f7f0000>):31)
% (short) Writing 1.e4,e6 starting games to: 'short/french.pgn'
% Closing chess db: '/home/nicos/short'
true.

% open file short/french.pgn on program that can play the games eg
linux> chessx short/french.pgn
==

---+++ Mini tutorial

The above example in detail.

Turn debugging of writing out the original games as they are added to the database.

==
?- debug( chess_db(original) ).
==

Create a database in fresh directory short/. The DB will be populated with games from pgn file chess_db/data/4ncl_short.pgn<br>
The database is closed after it is populated.

==
?- chess_db( pgn('4ncl_short'), short, create(true) ).
.....
1. e4 e6 2. d4 d5 3. Nd2 Be7 4. Bd3 c5 5. dxc5 Nf6 6. Qe2 O-O 7. Ngf3 a5 8. O-O
Na6 9. e5 Nd7 10. Nb3 Ndxc5 11. Bb5 Bd7 12. Bxd7 Qxd7 13. Nbd4 Ne4 14. Be3 f5
15. Qb5 Qxb5 16. Nxb5 Rfc8 17. c3 Nac5 18. Nfd4 Ra6 19. f3 Ng5 20. Rad1 Nf7 21.
f4 Ne4 1/2-1/2

true.
==

Connect to the new database. The connections are managed internally.

==
?- chess_db_connect( short, profile(false) ).
true.
==

Interrogate all connected databases for games starting with the sequence [e4,e6] (French defence).

==
?- findall( Gid, (chess_db_opening([e4,e6],Gid),write(Gid),nl), Gids ).
chdbs(<sqlite>(0x276c930),<sqlite>(0x278f320),<sqlite>(0x2792450)):1
chdbs(<sqlite>(0x276c930),<sqlite>(0x278f320),<sqlite>(0x2792450)):31
Gids = [chdbs(<sqlite>(0x276c930), <sqlite>(0x278f320), <sqlite>(0x2792450)):1, chdbs(<sqlite>(0x276c930), <sqlite>(0x278f320), <sqlite>(0x2792450)):31].
==

Turn general library debugging on.

==
?- debug(chess_db(info)).
==

Re-connecting is handled fine.

==
?- chess_db_connect( short, profile(false) ).
% Handles already exist, for chess_db directory: '/home/nicos/short'
==

Create a new PGN file from the original scripts of the two French defence 2 games in their original script. Moves are matched to game_move/4 and are pulled from the game_orig/2 sub-database.

==
?- PgnF = 'short/french.pgn', chess_db_opening_pgn( [e4,e6], PgnF ).
==

View the two games in a PGN interface program such as:

==
?- shell( 'chessx short/french.pgn' ).
==

---+++ Debug terms

Listens to:
  * chess_db(info)
     trigger light reporting across the library
  * chess_db(move)
  * chess_db(moves)
  * chess_db(original)
  * chess_db(true) 
     (on-by-default channel, turn off for silent operation)

---+++ Pack predicates

parsing of and saving to pgn files, and storing/retriving on/from chess_dbs
  * pgn/2
  * chess_db/1,chess_db/2,chess_db/3
  * chess_db_ids_pgn/2

manage database connections
  * chess_db_connect/1,chess_db_connect/2
  * chess_db_disconnect/0, chess_db_disconnect/1
  * chess_db_current/1,chess_db_current/2

access db games and info
  * chess_db_game/1
  * chess_db_game_info/3
  * chess_db_opening/2
  * chess_db_match/2

info interactions
  * chess_db_list/1
  * chess_db_max_id/2

miscellaneous
  * chess_db_version/2

---+++ Pack info

@author nicos angelopoulos
@version  0.1 2018/3/18
@version  0.2 2018/3/20
@version  0.3 2018/9/14
@see  http://stoics.org.uk/~nicos/sware/chess_db
@see  https://github.com/nicos-angelopoulos/chess_db
@see  [pack('chess_db/examples/short.pl')]
@see  pack(prosqlite), pack(db_facts), pack(stoics_lib), pack(options)
@tbd  discard illegal moves when considering possible ones

*/

/** chess_db_version( -Version, -Date ).

     The current version. Version is a Mj:Mn:Fx term, and date is a date(Y,M,D) term.

==
?- chess_db_version( 1:0:0, date(2020,3,30) ).
true.
==
*/
chess_db_version( 1:0:0, date(2020,3,30) ).
