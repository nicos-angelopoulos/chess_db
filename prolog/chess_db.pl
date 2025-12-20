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
                        chess_db_position/3,                    % +PosOrMoves, -DbHandles, -Conts
                        % db.info
                        chess_db_list/1,                        % +Dir
                        chess_db_max_id/2,                      % +HandleST, -Max
                        % chess.dict
                        chess_dict_start_board/1 ,              % -Brd
                        chess_dict_inpo/2,                      % ?Brd, ?Inpo
                        chess_pgn_limos/2,                      % +PgnTerm, -Limos
                        chess_dict_move/3,chess_dict_move/4,    % +Move, +DictI, [+Turn,] -DictO
                        chess_dict_piece/3,                     % +DictPiece, +Colour, +Piece
                        chess_annotate_freq/3,                  % +Pgn, +CheDbLoc, +Args
                        chess_fen_square/2,                     % ?Fen, ?Sqr
                        chess_dict_pos_algebraic/2,             % ?Pos, ?Alg
                        % etc
                        chess_db_version/2                      % -Vers, -Date
                     ] ).

chess_db_alias( Alias, Path ) :-
    user:file_search_path( Alias, Path ),
    !.
chess_db_alias( Alias, Path ) :-
    assertz( user:file_search_path(Alias,Path) ).

:- multifile(user:file_search_path/2).
:- chess_db_alias(pgn, pack('chess_db/data/pgn')).
:- chess_db_alias(chess_db, pack('chess_db/data/dbs')).

% user:file_search_path( pgn, pack('chess_db/data/pgn') ).
% user:file_search_path( pgn, pack('chess_db_data/pgn') ).
% user:file_search_path( chess_db, pack('chess_db_data/dbs') ).

:- use_module(library(apply)).       % include/3,...
:- use_module(library(lists)).       % append/3,...
:- use_module(library(debug)).       % debug/1,3.  % should replace with debuc/1,3
:- use_module(library(pure_input)).  % phrase_from_file/2.

:- use_module(library(lib)).

:- lib(options).
:- lib(os_lib).
:- lib(db_facts).
:- lib(prosqlite).
:- lib(stoics_lib).
:- lib(debug_call).
:- lib(pack_errors).

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
:- lib(chess_dict_move/4).
:- lib(chess_pgn_limos/2).
:- lib(chess_annotate_freq/3).
:- lib(chess_dict_pos_algebraic/2).
:- lib(chess_db_position/3).

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
?- pack_install(chess_db).
==

There are three example pgns in pack(chess_db/data/pgn) and an example program in pack(chess_db/examples/short.pl).

The best way to view the documentation is via the SWI-Prolog html documentation server:

==
?- doc_server(3004).
% Started server at http://localhost:3004/pldoc/
true.

?- use_module(library(chess_db)).
true.

?- www_open_url('http://localhost:3004/pldoc' )
% Which will open a browser pointing to the doc server. At this page search for the term: chess_db.

==

Static docs version is available at: https://stoics.org.uk/~nicos/sware/chess_db/doc/html/chess_db.html

Home page: https://stoics.org.uk/~nicos/sware/chess_db

The source code for the pack is available at github: https://github.com/nicos-angelopoulos/chess_db


---+++ Dependencies

Packs:
  * [prosqlite](http://stoics.org.uk/~nicos/sware/prosqlite)   (1.6)
  * [db_facts](http://stoics.org.uk/~nicos/sware/stoics_lib)   (0.5)
  * [stoics_lib](http://stoics.org.uk/~nicos/sware/stoics_lib) (>= 1.9)
  * [options](http://stoics.org.uk/~nicos/sware/options)       (1.4)
  * [debug_call](http://stoics.org.uk/~nicos/sware/debug_call) (1.4)

Latest test on SWI-Prolog 9.3.34 (2025/12/04).

---+++ Basics

The pack includes code to:
  * parse
     PGN's: pgn/2
  * store
    the parsed terms into prosqlite databases: chess_db/2
  * interact
     with the resulting databases: chess_db_game_info/3, chess_db_opening_pgn/2
  * dictionary 
     representation of chess games: chess_dict_start_board/1, chess_dict_move/3
  * integer 
    representation of game positions: chess_dict_inpo/2
  * logic
    for identifying pinned pieces (used in move disambiguation)

See below for details on each.

A number of chess databases can be connected at the same time. Operations
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

Turn debugging on- writing out the original games as they are added to the database.

==
?- debug(chess_db(original)).
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

Find continuations for specific positions
==
?- chess_db_connect( short, profile(false) ).
?- chess_db_position( [e4,e6], Handles, Conts ).
Handles = chdbs(<sqlite>(0xaaaae1482af0), <sqlite>(0xaaaae1487890), <sqlite>(0xaaaae148c740), <sqlite>(0xaaaae1491500)),
Conts = '1-3-d3;31-3-d4' ;
false
==

Please note that this is more generic than chess_db_opening/2 as it also finds transpositions.

---+++ Debug terms

Listens to:
  * chess_db(info)
     trigger light reporting across the library
  * chess_db(move)
  * chess_db(moves)
  * chess_db(original)
  * chess_db(true) 
     (on-by-default channel, turn off for silent operation)


---+++ PGNs

Portable Game Notation is the standard for recording chess games. Each file, typically with a .pgn extension, may contain multiple games.
In most cases a single PGN file will contain all the games a particular chess tournament.

For example the file =|pack(chess_db('data/pgn/4NCL-2017.pgn')|= records some of the  games for the 2017, 4NCL league taking place in the UK.

This pack implements a parser for PGNs (pgn/2) that converts the games in the .pgn file to a Prolog terms list, where each game is 
represented by a =|pgn(Info,Moves,Res,Orig)|= term (see pgn/2).

The following two queries are equivalent
==
?- pgn( pack(chess_db/data/pgn/'4NCL-2017.pgn'), Pgn ).
Pgn = [pgn(['Event'-'4NCL Division 1a',...]),...]

?- pgn( pgn('4NCL-2017.pgn'), Pgn ).
Pgn = [pgn(['Event'-'4NCL Division 1a',...]),...]
==

To disambiguate between multiple pieces that maybe able to be refered to by a game move, the pack implements
a pinned piece logic. The non-interface predicate implementing this is: chess_dict_move_pin/3.

Disambiguation is needed in the following example.
On a board with white rooks on _b2_ and _b7_ a move noded by Rb3, could refer to either of the rooks moving to this square.
If both rooks are unconstrained the move should have be recordes as either _R2b3_ or _R7b3_. If the move is recorded as _Rb3_, 
it means that either of the two rooks is constrained by a pin: ie moving the rook would put the white king under attack.
For instance, if the white king is at _a1_ and the black queen is on _c3_, then the rook on _b2_ is pinned and 
moving it to _b3_ whould be an illegal move. Thus, in this context _Rb3_ refers to moving the rook on _b7_ to _b3_.

---+++ Databases

By default, each chess_db database directory contains 4 SQLite DBs each holding a single table.
In the following, a + sign prefixes a key field:

  * game_info.sqlite
     contains game_info(+Gid,+Key,Val) info Key->Val pairs about each game

  * game_move.sqlite
     table is, game_move(+Gid,+Ply,Hmv,Move)
     * Gid is the game id
     * Ply is the ply move for Move
     * Hmv is the half moves since last take (0 if Move is a take, or in start board)
     * Move is the original, algebraic, form of the move)

  * game_orig.sqlite
     table is game_orig(+Gid,Orig); where Orig is the verbatim of the section in the PGN for that game

  * game_posi.sqlite
     table is game_posi(+Posi,GPPairs), where GPPairs is a ; seperated Gid-Ply-Move pairs stored as text (eg '1-2-e4;2-4-e3'), Posi
     is a long integer stored as a string

  * game_posi.sqlite 
    table is game_posi(Posi,Conts); where Posi is a unique position and Conts is the continuations string (Gid-Ply-Mov, eg: 1-3-d5).

Positions are encoded as long integers.

A number of chess_dbs can be opened at the same time. 

Databases are created and managed with packs library(db_facts) and library(prosqlite).

---+++ Dictionaries

The pack also implements boards as dictionaries along with predicates that effect transitions to the board 
due to moves. 

Positions on the board get keys 1-64, and there are keys for: whether various castlings and any en passant are still valid, 
whose move it is, ply move and half moves since last take.

See: 
  * chess_dict/0 (doc predicate),
  * chess_dict_start_board/1 gets the starting board
  * chess_dict_move/3 enact a move on a board
  * chess_dict_piece/3 tabulates numeric id of piece to colour and piece name (atoms)

---+++ Inpos

Integer representation of positions. These hold less information than a board dictionary as we
want to make positions that look the same identical, irrespective of when was the last take or 
at which stage (ply counter) of the game the position arose.

To convert from a dictionary use: chess_dict_inpo/2.

Inpos are used as the first field of game_posi/2 database tables (see Databases section above).


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
  * chess_db_position/3
  * chess_db_match/2

dictionaries
  * chess_dict/0
  * chess_dict_start_board/1
  * chess_dict_move/3
  * chess_dict_piece/3

info interactions
  * chess_db_list/1
  * chess_db_max_id/2

documentation
  * chess_dict/0
  * chess_db_version/2

---+++ Pack info

@author nicos angelopoulos
@version  0.1 2018/3/18
@version  0.2 2018/3/20
@version  0.3 2018/9/14
@version  1.0 2021/6/18
@see  http://stoics.org.uk/~nicos/sware/chess_db
@see  https://github.com/nicos-angelopoulos/chess_db
@see  [pack('chess_db/examples/short.pl')]
@see  pack(prosqlite), pack(db_facts), pack(stoics_lib), pack(options)
@tbd  discard illegal moves when considering possible ones

*/

/** chess_db_version( -Version, -Date ).

     The current version. Version is a Mj:Mn:Fx term, and date is a date(Y,M,D) term.

==
?- chess_db_version(1:0:0, date(2021,6,18)).
true.
==
*/
chess_db_version(1:0:2, date(2025,12,8)).   % switched implementation of chess_dict_inpo/2
