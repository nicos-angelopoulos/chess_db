
:- lib(chess_db).
:- lib(rocksdb).

:- lib(by_unix).
:- lib(debug_call).

:- debug(rocks_ex).
:- debug(chess_db).

rocks_ex:-
     rocks_ex( [] ).

rocks_ex( _Opts ) :-
     Self = rocks_ex,
     lib( rocksdb ),
     tmp_file( rock_chess_db_18_3, Tmp ),
     debuc( Self, 'Tmp rocks db dir: ~p', Tmp ),
     chess_db( pack('chess_db/data/pgn/18.03-candidates.pgn'),  Tmp, create(true) ),
     assert( rocks_ex_tmp_dir(Tmp) ).

rocks_ex_enum_gids :-
     rocks_ex_handle( info, InfoHd/InfoRv ),
     findall( _, (rocks_enum(InfoHd,K,V),writeln(K-V)), _ ),
     findall( _, (rocks_enum(InfoRv,K,V),writeln(K-V)), _ ).

rocks_ex_enum_moves :-
     rocks_ex_handle( move, MoveHd ),
     findall( _, (rocks_enum(MoveHd,K,V),writeln(K-V)), _ ).

rocks_ex_enum_origs :-
     rocks_ex_handle( orig, OrigHd ),
     findall( _, (rocks_enum(OrigHd,K,V),writeln(K-V)), _ ).

rocks_ex_enum_posis :-
     rocks_ex_handle( posi, PosiHd ),
     findall( _, (rocks_enum(PosiHd,K,V),writeln(K-V)), _ ).

rocks_ex_handle( Section, Handle ) :-
     Self = rocks_ex,
     rocks_ex_tmp_dir( Tmp ),
     % debuc( Self, 'Got tmp rocks db dir: ~p', Tmp ),
     % @ ls( Tmp ),
     % once( chess_db:chess_db_handles(Tmp,CdbHs) ),
     once( chess_db_current(CdbHs,Tmp) ),
     % debuc( Self, 'Got tmp rocks db handles: ~w', CdbHs ),
     chess_db:chess_db_handle( Section, CdbHs, Handle ),
     debuc( Self, 'Got ~w, handle: ~w', [Section,Handle] ).
