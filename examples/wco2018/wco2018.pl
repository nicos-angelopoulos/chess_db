
:- use_module( library(lib) ).

:- lib(os_lib).
:- lib(by_unix).
:- lib(chess_db).

:- lib(stoics_lib:url_file/3).
:- lib(stoics_lib:n_digits_min/3).

:- debug(wco2018).

/**  wco2018.

Builds a chess database from the 2018's WCO PGN zip file.
Shows some examples of queries for generating sub-PGNs.

  * opt(Opt=_)
     is a...

==
?- [pack('chess_db/examples/wco2018/wco2018.pl')].
?- wco2018_build.
% how many games, are there ?
?- wco2018_len(Len).
Len = 4036.
% create a Pgn with all the Caro Cann games
?- wco2018_caro_cann.
% play the games in a GUI front end...
?- @ chessx( 'wco2018_caro_cann.pgn' ).
==

@author nicos angelopoulos
@version  0.1 2018/10/07

*/

wco2018_len( Len ) :-
    chess_db_connect( chess_db('wco2018.chess') ),
    findall( Gid, chess_db_game(Gid), Gids ), length( Gids, Len ),
    chess_db_disconnect.
    
wco2018_caro_cann :-
    chess_db_connect( chess_db('wco2018.chess') ),
    chess_db_opening_pgn( [e4,c6], 'caro_cann.pgn' ),
    chess_db_disconnect.

wco2018_build :-
    Self = wco2018,
    wco2018_downloads_dir( DnD ),
    wco2018_alias( pgn(Self), PgD ),
    os_ext( chess, Self, Celf ),
    wco2018_alias( chess_db(Celf), WcoDB ),
    %
    debug( Self, 'Downloads directory: ~p', DnD ),
    debug( Self, 'PGN directory: ~p', PgD ),
    debug( Self, 'chess_db directory: ~p', WcoDB ),
    %
    MkOpts = [debug(true),debug_exists(true)],
    os_make_path( DnD, MkOpts ),
    os_make_path( PgD, MkOpts ),
    %
    ZipF = 'wco2018open11.zip',
    wco2018_build( Self, ZipF, DnD, PgD, WcoDB ).

wco2018_build( Self, _ZipF, _DnD, _PgnD, ChessDB ) :-
    exists_directory( ChessDB ),
    debug( Self, 'SQLite database files already exist. Quiting the rest of the build.', true ),
    !.
wco2018_build( Self, _ZipF, _DnD, PgnD, ChessDB ) :-
    working_directory( Old, PgnD ),
    findall( PGN, (between(1,11,I),n_digits_min(2,I,Ia),
                   atomic_list_concat(['wco2018_r',Ia,'_open.pgn'],'',PGN)
                  ), PGNs ),
    debug( Self, 'Testing for PGNs: ~w', [PGNs] ) ,
    ( maplist(os_exists,PGNs) ->   
            % depends on default option: err=test, 
            % see, stoics_lib:map_list_options/3,4 if you need to use the options explicitly
            true
            ;
            working_directory( _, Old ),
            fail
    ),
    debug( Self, 'Round pgns already exist, skipping downdload and unzip steps', true ),
    !,
    % wco2018_build_db( Self, ZipF, DnD, PgnD, ChessDB ).
    working_directory( _, PgnD ),
    wco2018_build_pgns( Self, ChessDB ),
    working_directory( _, Old ).
wco2018_build( Self, ZipF, _DnD, PgnD, ChessDB ) :-
    os_path( PgnD, ZipF, ZipP ),
    exists_file( ZipP ),
    !,
    debug( Self, 'Zip file in pgns directory already exists, skipping downdload and copy steps', true ),
    wco2018_build_unzip( Self, ZipF, PgnD, ChessDB ).
wco2018_build( Self, ZipF, DnD, PgnD, ChessDB ) :-
    os_path( DnD, ZipF, DnZipP ),
    exists_file( DnZipP ),
    debug( Self, 'Zip file already exists in downloads, skipping downdload step', true ),
    !,
    os_path( PgnD, ZipF, PgnZipP ),
    copy_file( DnZipP, PgnZipP ),
    wco2018_build_unzip( Self, ZipF, PgnD, ChessDB ).

wco2018_build( Self, ZipF, DnD, PgnD, ChessDB ) :-
    % step 1. download the zip file (if not already done so)
    working_directory( Old, DnD ),
    Url = 'https://batumi2018.fide.com/storage/app/media/pgn/wco2018open11.zip',
    ( url_file(Url,File,overwrite(fail)) ->
        debug( wco2018, 'Local file: ~p', File )
        ;
        debug( wco2018, 'Zip file already existed. Did not download it again', true )
    ),
    % step 2. Unzip the zip file (if some of the PGN files are missing
    os_path( PgnD, ZipF, PgnZipP ),
    copy_file( File, PgnZipP ),
    working_directory( _, Old ),
    wco2018_build_unzip( Self, ZipF, PgnD, ChessDB ).

wco2018_build_unzip( Self, ZipF, PgnD, ChessDB ) :-
    working_directory( Old, PgnD ),
    ls,
    @ unzip( ZipF ),
    @ rm( -f, ZipF ),
    working_directory( _, PgnD ),
    wco2018_build_pgns( Self, ChessDB ),
    % wco2018_build_db( Celf, Self, DnD, ZiPF, PgnD ).
    working_directory( _, Old ).

wco2018_build_pgns( Self, ChessDB ) :-
    os_file( PgnF ),
    os_ext( pgn, PgnF ),
    atom_concat( 'wco2018_r', _, PgnF ),
    debug( Self, 'Adding to db: ~p', PgnF ),
    ( chess_db(PgnF,ChessDB,create(true)) -> true; throw(failed_pgn(PgnF)) ), 
    fail.
wco2018_build_pgns( _Self, _ChessDB ).

wco2018_alias( Alias, Loc ) :-
    % absolute_file_name( Alias, Loc ),
    ( ( ( absolute_file_name(Alias,Loc,[solutions(all)]),
        os_path(ChessDbDataDbsD,_,Loc),
        os_path(ChessDbDataD,_,ChessDbDataDbsD),
        os_path(PackD,_,ChessDbDataD),
        exists_directory(PackD)
       )
        ;
        absolute_file_name(Alias,Loc,[file_type(directory)])
       )
            -> true; throw(error1(Alias))
    ).

wco2018_downloads_dir( DnD ) :-
    catch( os_cast(chess_db(dnloads),DnD),_,fail),
    exists_directory( DnD ),
    !.
wco2018_downloads_dir( DnD ) :-
    catch( os_cast(chess_db('../dnloads'),DnD),_,fail),
    exists_directory( DnD ),
    !.
wco2018_downloads_dir( DnD ) :-
    catch( os_cast(chess_db('Downloads'),DnD),_,fail),
    exists_directory( DnD ),
    !.
wco2018_downloads_dir( DnD ) :-
    os_cast( pack('Downloads'), +DnD ).

% work in progress
% the Url below would seem to create a multi sheet spreadsheet, 
% with only one live sheet. can convert with ssconvert -S wco2018open-teams.xls wco2018open-teams.csv
% or you can convert with % unoconv -f ods *.xlsx
% for now include csv in pack ?
%
wco2018_teams :-
    wco2018_downloads_dir( DnD ),
    os_path( DnD, 'wco2018open-teams.xls', Xls ),
    Url = 'http://chess-results.com/tnr368908.aspx?lan=1&art=1&flag=30',
    ( url_file(Url,Xls,overwrite(false)) -> true; true ),
    os_ext( _, csv, Xls, Csv ),
    debug( by_unix ),
    ( os_exists(Csv) ->
        true
        ;
        @ ssconvert( Xls, Csv )
    ),
    write( csv(Csv) ), nl,
    mtx( Csv, Mtx, match(false) ),
    write( mtx(Mtx) ), nl.
