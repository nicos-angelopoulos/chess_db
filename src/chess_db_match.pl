/**  chess_db_match( +Match, ?Games ).

Select a number of games from the open databases according to their info labels.
If Games is a variable each matching Gid is returned on backtracking. Else, is
taken to be a pgn file (via chess_db_connect/2), to which the originals from
all matching games are dumped. Match is a list of the following type of terms. 
The list items are considered as a disjunction.

Match-terms
 * atom (exact match to any info label)
 * + terms (conjuction of the plused terms)
 * Key(InfoMatch) where only Key keys are (no-cased) matches with InfoMatch
   ( -Key means do not match Key's case)
   * atom (exact match)
   * -atom (no case match)
   * sub(Atom) where Atom is matched with sub_atom/2
   * sub(- Atom) where Atom is matched with sub_atom/2 and no cased

==
?- chess_db_connect( chess_db(gms) ).
?- chess_db_match( 'So, Wesley', count(Count) ).
Count = 40
?- chess_db_match( [-white(sub('So'))], Gid ), Gid = _:Gno, chess_db_id_info(Gid,'White',White), write( Gno:White ), nl, fail.
...
196:So,W
...
305:So, Wesley

?- chess_db_match( [-white(sub('So'))], count(Count) ).
Count = 27.

?- chess_db_match( [-black(sub('So'))], count(Count) ).
Count = 27.

chess_db_match( [-black(sub('So'))], Gid ), Gid = _:Gno, chess_db_id_info(Gid,'White',White), write( Gno:White ), nl, fail.
6:Caruana, Fabiano
16:Topalov, Veselin
...

?- chess_db_match( [white(sub('So'))], count(Count) ).
Count = 0.

?- chess_db_match( ['White'(sub('So'))], count(Count) ).
Count = 27.

?- chess_db_match( sub('So,'), count(Count) ).
?- chess_db_match( sub('So,'), 'SoWesley' ).
==

@author nicos angelopoulos
@version  0.1 2018/8/19

*/
chess_db_match( MatcH, Games ) :-
    en_list( MatcH, Match ),
    !,
    chess_db_info_match( Match, Games ).

chess_db_info_match( Match, Gid ) :-
    var( Gid ),
    !,
    chess_db_info_match_list( Match, Gid ).
chess_db_info_match( Match, count(Count) ) :-
    !,
    findall( Gid, chess_db_info_match_list(Match,Gid), Gids ),
    length( Gids, Count ).
chess_db_info_match( Match, Games ) :-
    findall( Gid, chess_db_info_match_list(Match,Gid), Gids ),
    chess_db_ids_pgn( Gids, Games ).

chess_db_info_match_list( List, Gid ) :-
    chess_db_game_id( Gid ),
    % fixme: this is quite wasteful, ...:
    findall( Key=Val, chess_db_info(Gid,Key,Val), Info ),
    chess_db_info_match_list_info( List, Info ).

chess_db_info_match_list_info( [M|_Ms], Info ) :-
    chess_db_info_match_game_disjunct( M, Info ), 
    !.
chess_db_info_match_list_info( [_M|Ms], Info ) :-
    chess_db_info_match_list( Ms, Info ).

chess_db_info_match_game_disjunct( A+B, Info ) :-
    !,
    chess_db_info_match_game_disjunct( A, Info ),
    chess_db_info_match_game_disjunct( B, Info ).

chess_db_info_match_game_disjunct( -A, Info ) :-
    !,
    chess_db_info_match_game_un_cased( A, Info ).
chess_db_info_match_game_disjunct( Atom, Info ) :-
    atom( Atom ),
    !,
    memberchk( _Key=Atom, Info ).
chess_db_info_match_game_disjunct( sub(A), Info ) :-
    !,
    chess_db_info_match_game_any_sub( A, Info ).
chess_db_info_match_game_disjunct( Compound, Info ) :-
    functor( Compound, Name, 1 ),
    !,
    arg( 1, Compound, Arg ),
    member( Name=Val, Info ),
    chess_db_info_match_value( Arg, Val ).
    % chess_db_info_match_game_key( Name, Arg, Info ).
chess_db_info_match_game_disjunct( Other, _Gid ) :-
    throw( cannot_translate_match_pattern(Other) ).

chess_db_info_match_game_key( Name, - Arg, Info ) :-
    !,
    atom( Arg ),
    downcase_atom( Arg, Drg ),
    chess_db_info_match_game_key_un_cased( Name, Drg, Info ).

chess_db_info_match_game_un_cased( A, Info ) :-
    atom( A ), 
    !,
    downcase_atom( A, D ),
    chess_db_info_match_game_any_un_cased( Info, D ).
chess_db_info_match_game_un_cased( sub(A), Info ) :-
    !,
    downcase_atom( A, D ),
    chess_db_info_match_game_any_sub_un_cased( Info, D ).
chess_db_info_match_game_un_cased( Comp, Info ) :-
    functor( Comp, Name, 1 ),
    !,
    arg( 1, Comp, Arg ),
    chess_db_info_match_game_key_un_cased( Name, Arg, Info ).
chess_db_info_match_game_un_cased( Other, _Info ) :-
    throw( cannot_match_uncased(Other) ).

chess_db_info_match_game_any_sub_un_cased( [_Key=Val|_T], D ) :-
    downcase_atom( Val, Dal ),
    sub_atom( Dal, D ),
    !.
chess_db_info_match_game_any_sub_un_cased( [_H|T], D ) :-
    chess_db_info_match_game_any_sub_un_cased( T, D ).

chess_db_info_match_game_any_un_cased( [_Key=Val|_T], D ) :-
    % fixme: debug shows what matched
    downcase_atom( Val, Dal ),
    D == Dal,
    !.
chess_db_info_match_game_any_un_cased( [_H|T], D ) :-
    chess_db_info_match_game_any_un_cased( T, D ).

chess_db_info_match_game_key_un_cased( MatchName, Arg, Info ) :-
    member( Name=Val, Info ),
    downcase_atom( Name, MatchName ),
    chess_db_info_match_value( Arg, Val ),
    !.

chess_db_info_match_game_any_sub( -A, Info ) :-
    !,
    downcase_atom( A, D ),
    chess_db_info_match_game_any_un_cased( Info, D ).
chess_db_info_match_game_any_sub( A, Info ) :-
    member( _Key=Val, Info ),
    sub_atom( Val, A ),
    !.

chess_db_info_match_value( Var, Val ) :-
    var( Var ),
    !,
    Val = Var.
chess_db_info_match_value( sub(Sub), Val ) :-
    chess_db_info_match_value_sub( Sub, Val ).
chess_db_info_match_value( -Arg, Val ) :-
    chess_db_info_match_value_un_cased( Arg, Val ).

chess_db_info_match_value_sub( -Arg, Val ) :-
    !,
    downcase_atom( Arg, Drg ),
    downcase_atom( Val, Dal ),
    sub_atom( Dal, Drg ).
chess_db_info_match_value_sub( Arg, Val ) :-
    sub_atom( Val, Arg ).

chess_db_info_match_value_un_cased( sub(Sub), Val ) :-
    !,
    downcase_atom( Sub, Dub ),
    downcase_atom( Val, Dal ),
    sub_atom( Dal, Dub ).
chess_db_info_match_value_un_cased( Atom, Val ) :-
    downcase_atom( Atom, Down ),
    downcase_atom( Val, Down ).
