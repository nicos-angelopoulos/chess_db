
:- lib(os_lib).
:- lib(by_unix).
:- lib(chess_db).

:- lib(stoics_lib:url_file/3).
:- debug(wco2018).

/**  wco2018.

(Can) get the wco PGN files and builds a proSQLite database from them.
Shows some examples of queries for genereting sub-PGNs.

  * opt(Opt=_)
     is a...

==
?- wco2018_build.
?- wco2018.
==

@author nicos angelopoulos
@version  0.1 2018/2/

*/
wco2018_build :-
    os_cast( pack(chess_db/examples/wco2018), +ExD ),
    os_path( ExD, dnload, DnD ),
    os_make_path( DnD, [debug(true),debug_exists(true)] ),
    working_directory( Old, ExD ),
    SQLite = wco2018.sqlite,
    wco2018_build_sqlite( SQLite, DnD ),
    working_directory( _, Old ).

wco2018_build_sqlite( SQLite, _DnD ) :-
    exists_file(SQLite) ->
    debug( wco2018, 'SQlite file already exists. Quiting the rest of the build.', true ),
    !.
wco2018_build_sqlite( SQLite, DnD ) :-
    working_directory( _, DnD ),
    File=wco2018open11.zip,
    Url = 'https://batumi2018.fide.com/storage/app/media/pgn/wco2018open11.zip',
    ( url_file(Url,File,overwrite(fail)) ->
        debug( wco2018, 'Local file: ~p', File )
        ;
        debug( wco2018, 'Zip file already existed. Did not download it again', true )
    ),
    ( exists_file( 'wco2018_r11_open.pgn') -> 
        debug( wco2018, 'Round pgns seem to already exist, skipping unzip step', true )
        ;
        debug( wco2018, 'Unpacking the zip file.', true ),
        os_ext( zip, File, Stem ),
        @ unzip( Stem )
    ),
    os_path( '..', SQLite, PaSQLite ),
    wco2018_build_pgns( PaSQLite ).

wco2018_build_pgns( SQLite ) :-
    os_file( PgnF ),
    os_ext( pgn, PgnF ),
    debug( wco2018, 'Adding to db: ~p', PgnF ),
    chess_db( PgnF, SQLite, create(true) ),
    fail.
wco2018_build_pgns( _SQLite ).
