
:- lib(db_facts).
:- lib(prosqlite).

:- lib(chess_db).

caro :-
    working_directory( Here, '/usr/local/users/chess/chess_db/dbs/gms/' ),
    assert( here(Here) ),
    sqlite_connect( 'game_posi.sqlite', Cpos ), assert( conn_posi(Cpos) ),
    sqlite_connect( 'game_orig.sqlite', Cori ), assert( conn_orig(Cori) ),
    sqlite_connect( 'game_move.sqlite', Cmov ), assert( conn_move(Cmov) ),

    chess_dict_start_board( Bd_00 ),
    chess_dict_move( 'e4', Bd_00, Bd_01 ),
    chess_dict_move( 'c6', Bd_01, Bd_02 ),
    chess_dict_inpo( Bd_02, C6Inpo ),

    findall( Gid-Ply, db_holds(game_posi(Gid,Ply,C6Inpo)), Glys ),
    write( glys(Glys) ), nl,

    Glys = [Gid1-Ply1|_], 

    once( db_holds( game_orig(Gid1,Orig1) ) ),
    write( orig_1(Orig1) ), nl,

    once( db_holds( game_move(Gid1,Ply1,_Hmv,Next) ) ),
    write( next_move(Next) ), nl,


    findall( GidX-Nmv, ( db_holds(game_posi(GidX,PlyX,C6Inpo)),
                         db_holds(game_move(GidX,PlyX,_,Nmv))
                            ),
                                GMs ),
    write( next_moves(GMs) ), nl.
