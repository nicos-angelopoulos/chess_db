
:- use_module(library(lists)).  % append/3.
:- use_module(library(apply)).  % maplist/2.
:- use_module(library(lib)).    % stoics.org pack loader

:- lib(os_lib).     % os_postfix/4.
:- lib(options).
:- lib(chess_db).   % chess_db/3.
:- lib(debug_call).

:- lib(stoics_lib:kvs_k_update_v/7).
:- lib(stoics_lib:portray_clauses/2).

pgn_game_lengths_defaults( Defs ) :-
                              Defs  =  [
                                             debug(true),
                                             output(file),
                                             incr(true),
                                             stack_limit(false),
                                             postfix(glens),
                                             sep('_')
                                       ].

pgn_game_lengths_help( Self ) :-
     debuc( Self, 'help goes here', [] ).

pgn_game_lengths_usage( Self ) :-
     debuc( Self, 'upsh ~w debug=true cdb=elite_pgn output=file incr=true', [Self] ).

/** pgn_game_lengths(+OptsAndFiles).

Calculate lengths of games in PGN files.

Atoms in OptsAndFiles are taken to be PGN files.

This script illustrates how to use the chess_db/2,3 machinery to process PGN in arbitray ways (rather
than to just load PGN games to a database). This is trivial in the case where the PGN can be loaded 
in memory as you the read terms (from pgn/2) can be passed to the counting predicates implemented here
without resorting to using chess_db/2, however, it is useful when we are dealing with huge PGNs 
via incremental processiing (option incr(true)) in chess_db/3.

Opts
  * debug(Dbg=true)
    informational, progress messages
  * help(Help=false)
    help messsage and exit
  * incr(Incr=true)
    as in os_chess/2 option. The default here is true, as this script 
    needs the chess_db/2 machinery more.
  * stack_limit(Mem=false)
    or give an integer, to set the stack memory in GB
  * output(Out=file)
    could also use =|terminal|=
  * postfix(Psx=glens)
    postfix for output file, if used
  * sep(Sep='_')
    separator for postfix (see os_postfix/3)
  * stack_limit(Slm=false)
    or set to a number (stack limit will be set to Slm Gbs)
  * usage(Usg=false)
    usage message and exit

Opts are passed to chess_db/2.

Examples
==
?- pgn_game_lengths([]).
==

==
> upsh pgn_game_lengths output=terminal ../data/pgn/18.03-candidates.pgn

% Doing: '../data/pgn/18.03-candidates.pgn', output on: '../data/pgn/18.03-candidates_glens.pgn'
% Calling chess_db/2 options: [incr(false),goal(pgn_games_length),goal_iter([]),goal_return(_46328)]
% Predicate: chess_db/2 at 21:23:57 on 24th of Jan 2026 starting task: PGN load from: ../data/pgn/18.03-candidates.pgn.
% Predicate: chess_db/2 option selected: incr(false).
% Wrote on file: '/home/nicos/pl/packs/src/chess_db/data/pgn/18.03-candidates_glens.pgn'
% Finished: pgn_game_lengths

==

@author nicos angelopoulos
@version  0.1 2026/01/24

*/

pgn_game_lengths( Args ) :-
     Self = pgn_game_lengths,
     options_append( Self, Args, Opts, atoms(Atoms) ),
     options( '$oa_cont'(Cont), Opts ),
     options( output(Omt), Opts ),
     pgn_game_lengths_opts( Cont, Self, Omt, Atoms, Opts ),
     debuc( Self, end, true ).

pgn_game_lengths_opts(false, _Self, _Omt, _Atms, _Opts).
pgn_game_lengths_opts( true, Self, Omt, Oses, Opts ) :-
     options( stack_limit(Slm), Opts ),
     pgn_game_stack_limit( Slm, Self ),
     maplist( pgn_game_lengths_os(Omt,Self,Opts), Oses ).

pgn_game_stack_limit( false, Self ) :-
     debuc( Self, 'No change to stack limit.', true ).
pgn_game_stack_limit( Gbs, Self ) :-
     Lim is Gbs * 10 ^ 9,
     debuc( Self, 'Setting stack limit to: ~w', [Lim] ),
     set_prolog_flag( stack_limit, Lim ).

% fixme: add R plot; from own preds
% pgn_game_lengths_os( terminal, Self, _Opts, Os ) :-
pgn_game_lengths_os( terminal, Self, Opts, Os ) :-
     debuc( Self, 'Doing: ~p (terminal output)', Os ),
     CdbOpts = [goal(pgn_games_length),goal_iter([]),goal_return(Rtn)|Opts],
     debuc( Self, 'Calling chess_db/2 options: ~w', [CdbOpts] ),
     chess_db( Os, CdbOpts ),
     write( returned_value:Rtn ), nl.
pgn_game_lengths_os( file, Self, Opts, Os ) :-
     os_postfix( _, Os, Posted, Opts ),
     debuc( Self, 'Doing: ~p, output on: ~p', [Os,Posted] ),
     CdbOpts = [goal(pgn_games_length),goal_iter([]),goal_return(Rtn)|Opts],
     debuc( Self, 'Calling chess_db/2 options: ~w', [CdbOpts] ),
     chess_db( Os, CdbOpts ),
     portray_clauses( [games_moves_freq(Rtn)], [mode(write),file(Posted)] ),
     debuc( Self, wrote, Posted, path(abs) ).

pgn_games_length( PgnL, Giter, _Prg, _Pos, _Tos, _CdbHs, Niter ) :-
     pgn_game_length( PgnL, Giter, Niter ).

pgn_game_length( [], Giter, Niter ) :-
     Giter = Niter.
pgn_game_length( [pgn(_Info,Moves,_Res,_Org)|Ps], Giter, Niter ) :-
     ( once( append(_,[move(LstMv,_,_,_,_)], Moves ) ) ->  
          % there is 1 game without moves in elite.lichess.18.01
          true
          ;
          ( Moves==[] ->
               LstMv is 0
               ;
               throw( unexpected_term_for_moves(Moves) )
          )
     ),
     kvs_k_update_v( Giter, LstMv, pgn_game_v_val_1, pgn_game_v_plus_1, _V, _NewV, Liter ),
     pgn_game_length( Ps, Liter, Niter ).

pgn_game_v_plus_1( K, V, NewV, K-NewV ) :-
     NewV is V + 1.

pgn_game_v_val_1(K, 1, K-1).
