:- module( chess_db, [ 
                        chess_db/1,chess_db/2,                  % +PgnToF[, +Opts]
                        chess_db_list/1,                        % +Dir
                        chess_db_max_id/2,                      % +HandleST, -Max
                        chess_db_connect/1,                     % +Opts
                        chess_db_disconnect/1,                  % ?Db
                        chess_db_current/1, chess_db_current/2, % +CdbHs[, +Opts]
                        chess_db_openning/2,                    % +Moves, -Gid
                        chess_db_openning/3,                    % +Moves, -Gid, +Opts
                        chess_db_openning_pgn/2,                % +Moves, +PgnF
                        chess_db_version/2,                     % -Vers, -Date
                        pgn/2                                   % +PgnToF, ?PgnT
                     ] ).

:- use_module(library(lib)).

:- lib(options).
:- lib(db_facts).
:- lib(prosqlite).
:- lib(stoics_lib).
:- lib(debug_call).

:- dynamic(chess_db_handles/2).       % Dir, HandlesTerm

:- lib(source(chess_db),homonyms(true)).

:- lib(pgn/2).
:- lib(chess_db/2).
:- lib(chess_db_list/1).
:- lib(chess_db_max_id/2).
:- lib(chess_db_connect/1).
:- lib(chess_db_current/2).
:- lib(chess_db_openning/2).
:- lib(chess_db_disconnect/1).
:- lib(chess_db_openning_pgn/2).

:- lib(chess_db_handles/4).
:- lib(chess_db_messages/0).

:- lib(end(chess_db)).

/**  <module> PGN and chess game databases.

This library produces chess games databases from PGN files and provides some<br>
predicates for manipulating these databases.

Once connected to a number of chess_db databases, all kinds of information about the games <br>
can be interrogated. (see chess_db_openning/2 for an example).

Ideally we want to hook this pack to a web-page interface for playing the games as we select them.

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
     chess_db_openning_pgn/2

There are two example databases in data/ and an example program, examples/short.pl

---+++ Dependencies

Packs:
  * prosqlite   
     (1.5)
  * db_facts    
     (0.5)
  * stoics_lib  
     (1.0)
  * options     
     (1.0)
  * debug_call
     (1.1)

---+++ Basics

By default, chess database dirs contain 3 sqlite databases:
  * game_info.sqlite
     contains game_info(Gid,Key,Val) info about each game
  * game_move.sqlite
     table is of form game_move(Gid,Num,Turn,Move)
  * game_orig.sqlite
     table is game_orig(Gid,Orig); where Orig is the verbatim of the section in the PGN for that game

A number of databases can be connected at the same time.

For example a number of databases can be declared in dir/1 terms within file: <br>
~/.pl/chess_db_connect.pl  see options_append/3.

---+++ Example

==
?- [pack('chess_db/examples/short.pl')].
?- short.      % creates a chess_db in subdirectory short/ from data/4ncl_short.pgn
               % and displays the game ids for games that start with [e4,e6] (French defense)
...

?- french.     % creates a new pgn file from the base for the 2 games in short/ that start with e4,e6
% Handles already exist, for chess_db directory: '/home/nicos/pl/packs/src/chess_db/examples/short'
gid(chdbs(<sqlite>(0x71b4f0),<sqlite>(0xe577f0),<sqlite>(0xe13400)):1)
gid(chdbs(<sqlite>(0x71b4f0),<sqlite>(0xe577f0),<sqlite>(0xe13400)):31)
writting_to_file(short/french.pgn)
true.

% open file french.pgn on program that can play the games
==


---+++ Mini tutorial

The above example in detail.

==
?- debug( chess_db(original) ).
==

Spits the original games as they are added to the database.

==
?- chess_db( pack('chess_db/data/4ncl_short.pgn'), [dir('short'),create(true)] ).
.....
1. e4 e6 2. d4 d5 3. Nd2 Be7 4. Bd3 c5 5. dxc5 Nf6 6. Qe2 O-O 7. Ngf3 a5 8. O-O
Na6 9. e5 Nd7 10. Nb3 Ndxc5 11. Bb5 Bd7 12. Bxd7 Qxd7 13. Nbd4 Ne4 14. Be3 f5
15. Qb5 Qxb5 16. Nxb5 Rfc8 17. c3 Nac5 18. Nfd4 Ra6 19. f3 Ng5 20. Rad1 Nf7 21.
f4 Ne4 1/2-1/2

true.
==

Create a database in fresh directory short/. The DB will be populated with games from pgn file chess_db/data/4ncl_short.pgn<br>
The database is closed after populated.

==
?- chess_db_connect( [dir('short'),profile(false)] ).
true.
==

Connect to the new database. The connections are managed internally.

==
?- findall( Gid, (chess_db_openning([e4,e6],Gid),write(Gid),nl), Gids ).
chdbs(<sqlite>(0x276c930),<sqlite>(0x278f320),<sqlite>(0x2792450)):1
chdbs(<sqlite>(0x276c930),<sqlite>(0x278f320),<sqlite>(0x2792450)):31
Gids = [chdbs(<sqlite>(0x276c930), <sqlite>(0x278f320), <sqlite>(0x2792450)):1, chdbs(<sqlite>(0x276c930), <sqlite>(0x278f320), <sqlite>(0x2792450)):31].
==

Interrogate (all connected) databases for games starting with the sequence [e4,e6] (French defense).

==
?- debug(chess_db).
==

Turn general debug messaging on.

==
?- chess_db_connect( [dir('short'),profile(false)] ).
% Handles already exist, for chess_db directory: '/home/nicos/pl/packs/private/chess_db/short'
==

Re-connecting is handled fine.

==
?- PgnF = 'short/french.pgn', chess_db_openning_pgn( [e4,e6], PgnF ).
==

Creates a new pgn file containing 2 games in their original script with the PGN file.<br>
Moves are matched to game_move/4 and are pulled from game_orig/2.

==
?- halt.
% chessx short/french.pgn
==

You can view the two games in your favourite game PGN viewer. In this case chessx in Linux.

---+++ Debug terms

Listens to:
  * chess_db
  * chess_db(move)
  * chess_db(original)

---+++ Pack predicates


working with games
  * pgn/2
  * chess_db/2
  * chess_db_openning/2
  * chess_db_openning_pgn/2

info interactions
  * chess_db_list/1
  * chess_db_max_id/2

manage database connections

  * chess_db_connect/1
  * chess_db_disconnect/1
  * chess_db_current/2

miscellaneous
  * chess_db_version/2


---+++ Pack info

@author nicos angelopoulos
@version  0.1 2018/3/18
@version  0.2 2018/3/20
@see  http://stoics.org.uk/~nicos/sware/chess_db
@see  https://github.com/nicos-angelopoulos/chess_db
@see  [pack('chess_db/examples/short.pl')]
@see  pack(prosqlite), pack(db_facts), pack(stoics_lib), pack(options)

*/

/** chess_db_version( -Version, -Date ).

     The current version. Version is a Mj:Mn:Fx term, and date is a date(Y,M,D) term.

==
?- chess_db_version( 0:2:0, date(2018,3,20) ).
true.
==
*/
chess_db_version( 0:2:0, date(2018,3,20) ).
