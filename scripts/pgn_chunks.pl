
:- use_module(library(apply)).  % maplist/2.
:- use_module(library(lists)).  % append/3, member/2.
:- use_module(library(lib)).

:- lib(os_lib).
:- lib(options).
:- lib(debug_call).

:- lib(stoics_lib:io_lines/2).
:- lib(stoics_lib:io_sections/3).
:- lib(stoics_lib:n_digits_min/3).

% :- set_prolog_flag(stack_limit, 16_589_934_592).

pgn_chunks_defaults( Defs ) :-
                              Defs = [
                                        debug(true),
                                        games(100_000),
                                        order(sort),
                                        stack_limit(false),
                                        options_types([games-integer,order-oneof([atoms,pgns,sort])])
                                     ].

pgn_chunks_help( Self ) :-
     M1 = 'Chop a PGN file to a number of chunks, each contain a max number of games.',
     debuc( Self, M1, [] ).

pgn_chunks_usage( Self ) :-
     debuc( Self, 'upsh ~w debug=true games=100_000 [pgn=PgnF1] PgnF2', [Self] ).

/** pgn_chunks(+OptsAndPgns).

Chop a number of PGN files to a number of chunks, each contain a max number of games.

The last chunk will contain less than the max number.
Each PGN is chunkified on its own, files are not strewn together before chunkified.
Input PGNs can be given as pgn(PGNo) options or as atomic options.


Opts
  * debug(Dbg=true)
    informational, progress messages
  * games(NoGames=100_000)
    number of games in each chunk
  * help(Help=false)
    help messsage and exit
  * order(Order=sort)
    or one of =|atoms,pgns|= for giving priority
  * pgn(PgnF)
    PGN file to chunkify (multiple are allowed)
  * stack_limit(Slm=false)
    set the stack_limit prolog flag (set integer which will be translated to Gigabytes)
  * usage(Usg=false)
    usage message and exit

Following example will chunkify file1.pgn, file2.pgn, file3.pgn and file4 (which has no filename extension).
==
?- pgn_chunks([pgn(file1),file2,file3.pgn,file4]).
==

To test, you can use the PGN files that are distributed as examples with pack(chess_db).

Command line via pack(upsh).
==

> upsh pgn_chunks usage=true

> upsh pgn_chunks ../data/pgns/18.03-candidates.pgn games=10
% Writing on: '../data/pgn/18.03-candidates_cnk1.pgn'
% Writing on: '../data/pgn/18.03-candidates_cnk2.pgn'
% Writing on: '../data/pgn/18.03-candidates_cnk3.pgn'
% Writing on: '../data/pgn/18.03-candidates_cnk4.pgn'
% Writing on: '../data/pgn/18.03-candidates_cnk5.pgn'
% Writing on: '../data/pgn/18.03-candidates_cnk6.pgn'

> ls -l ../data/pgns/18*cnk*
-rw-r--r-- 1 nicos nicos 7349 Jan 31 13:09 ../data/pgn/18.03-candidates_cnk1.pgn
-rw-r--r-- 1 nicos nicos 7446 Jan 31 13:09 ../data/pgn/18.03-candidates_cnk2.pgn
-rw-r--r-- 1 nicos nicos 8396 Jan 31 13:09 ../data/pgn/18.03-candidates_cnk3.pgn
-rw-r--r-- 1 nicos nicos 7170 Jan 31 13:09 ../data/pgn/18.03-candidates_cnk4.pgn
-rw-r--r-- 1 nicos nicos 8525 Jan 31 13:09 ../data/pgn/18.03-candidates_cnk5.pgn
-rw-r--r-- 1 nicos nicos 4488 Jan 31 13:09 ../data/pgn/18.03-candidates_cnk6.pgn
==

@author nicos angelopoulos
@version  0.1 2026/01/30
@see SWI-Prolog packs at: https://eu.swi-prolog.org/pack/list

*/

pgn_chunks( Args ) :-
     Self = pgn_chunks,
     options_append( Self, Args, Opts, atoms(FAs) ),
     options( '$oa_cont'(Cont), Opts ),
     pgn_chunks_opts( Cont, Self, FAs, Opts ),
     debuc( Self, end, true ).

pgn_chunks_opts(false, _Self, _FAs, _Opts).
pgn_chunks_opts( true, Self, FAs, Opts ) :-
     options( games(Games), Opts ),
     debuc( Self, option, games(Games) ),
     findall( FP, member(pgn(FP),Opts), FPs ),
     options( order(Ord), Opts ),
     pgn_chunks_pgns( Ord, FAs, FPs, Fs ),
     debuc( Self, enum, inputs/Fs ),
     maplist( pgn_chunks_os(Games,Self), Fs ).

pgn_chunks_os( Games, Self, Inp ) :-
     ( os_exists(Inp) -> 
          Os = Inp
          ;
          os_ext( pgn, Inp, Os )
     ),
     debuc( Self, 'Doing: ~w', [Os] ),
     io_sections( Os, Secs, [separator([])] ),
     length( Secs, Len ),
     debuc( Self, 'Length of sections: ~d', [Len] ),
     Parts is ((Len/2) // Games) + 1,
     atom_codes( Parts, Corts ),
     length( Corts, Ligts ),
     debuc( Self, 'Parts: ~w', [Parts] ),
     n_digits_min( Ligts, 1, Padded ),
     atom_concat( cnk, Padded, Psfx ),
     os_postfix( Psfx, Os, CnkF ),
     open( CnkF, write, Out ),
     debuc( Self, 'Writing on: ~p', [CnkF] ),
     pgn_chunks_part( Secs, Games, Games, Out, Os, Ligts, 1, Self ).

pgn_chunks_part( [], _Rames, _Games, Out, _Os, _Ligts, _Part, _Self ) :-
     close( Out ).
pgn_chunks_part( [I,M|T], Rames, Games, Out, Os, Ligts, Part, Self ) :-
     io_lines( Out, I ), nl( Out ),
     io_lines( Out, M ), nl( Out ),
     ( Rames =:= 1 -> 
          Rem is Games,
          Qart is Part + 1,
          close( Out ),
          n_digits_min( Ligts, Qart, Padded ),
          atom_concat( cnk, Padded, Psfx ),
          os_postfix( Psfx, Os, CnkF ),
          open( CnkF, write, Nut ),
          debuc( Self, 'Writing on: ~p', [CnkF] )
          ; 
          Rem is Rames - 1,
          Part = Qart,
          Out = Nut
     ),
     pgn_chunks_part( T, Rem, Games, Nut, Os, Ligts, Qart, Self ).

pgn_chunks_pgns( atoms, FAs, FPs, Fs ) :-
     append( FAs, FPs, Fs ).
pgn_chunks_pgns( sort, FAs, FPs, Fs ) :-
     append( FPs, FAs, Fs ).
pgn_chunks_pgns( pgns, FAs, FPs, Fs ) :-
     append( FAs, FPs, FLs ),
     sort( FLs, Fs ).
