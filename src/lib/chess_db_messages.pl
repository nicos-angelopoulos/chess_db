/**  chess_db_messages.

A lib file with all messaging and message handling predicates.

@author nicos angelopoulos
@version  0.1 2018/3/20

*/
chess_db_messages.

chess_db_info_kvs( KVs, Mess, Args ) :-
    KVs \== [],
    ( memberchk( 'White'-White, KVs ) -> true; White = '$null' ),
    ( memberchk( 'Black'-Black, KVs ) -> true; Black = '$null' ),
    atomic_list_concat( [White,vs,Black], ' ', Mess ),
    Args = [].

pack_errors:message( no_handle(Table,Handles) ) -->
    ['No handle for table: ~w, in term ~w'-[Table,Handles]].
