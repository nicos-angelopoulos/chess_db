This library produces chess games databases from PGN files and provides some
predicates for manipulating these databases.

Once connected to a number of chess_db databases, all kinds of information about the games
can be interrogated. See chess_db_opening/2 for an example.

Ideally we want to hook this pack to a web-page interface for playing the games as we select them.

### Installation: 

```
?- pack_install(chess_db).
```

The pack includes code to:
  * parse PGN's 
     pgn/2
  * store the parsed terms into prosqlite databases
     chess_db/2
  * manipulate the resulting databases
     chess_db_opening_pgn/2

There are two example databases in data/ and an example program, examples/short.pl.

### Dependencies

These are all [stoics](https://stoics.org.uk/~nicos/sware/packs) packs, available from SWI-Prolog [packs installer](https://www.swi-prolog.org/pack/list):
  * lib
     (2.11)
  * prosqlite
     (1.5)
  * db_facts    
     (0.5)
  * debug_call
     (2.1)
  * options     
     (1.0)
  * stoics_lib  
     (1.0)

Pack lib, is the only dependency loaded at installation. The rest will be installed on first run (code in pack(lib)).

### Basics

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

### Example

```
?- [pack('chess_db/examples/short.pl')].
?- short.      % creates a chess_db in subdirectory short/ from data/4ncl_short.pgn
               % and displays the game ids for games that start with [e4,e6] (French defence)
...

?- french.     % creates a new pgn file from the base for the 2 games in short/ that start with e4,e6
% Handles already exist, for chess_db directory: '/home/nicos/pl/packs/src/chess_db/examples/short'
gid(chdbs(<sqlite>(0x71b4f0),<sqlite>(0xe577f0),<sqlite>(0xe13400)):1)
gid(chdbs(<sqlite>(0x71b4f0),<sqlite>(0xe577f0),<sqlite>(0xe13400)):31)
writting_to_file(short/french.pgn)
true.

% open file french.pgn on program that can play the games
```

### Pack info

* author nicos angelopoulos
* version  0.1 2018/3/18
* see  https://stoics.org.uk/~nicos/sware/chess_db
* see  https://github.com/nicos-angelopoulos/chess_db
* see  [pack('chess_db/examples/short.pl')]
* see  pack(prosqlite), pack(db_facts), pack(stoics_lib), pack(options)
