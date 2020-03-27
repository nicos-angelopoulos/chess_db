/** chess_fen_square( ?Fen, ?Square ).

As chess_algebraic_square/2 but allows null location which is used in FEN for en passat-able square.

==
?- chess_fen_square( '-', Sq ).
Sq = 0.

?- chess_fen_square( A1, 1 ).
A1 = a1.

==

@author nicos angelopoulos
@version  0:1 2020/3/26

*/
chess_fen_square( '-', 0 ) :- !.
chess_fen_square( Alg, Sqr ) :-
    chess_algebraic_square( Alg, Sqr ).

% chess_algebraic_square( +Alg, -Sqr ).
% chess_algebraic_square( -Alg, +Sqr ).
%
% Convert between (an atomic) algebraic representation and a single number (dict.board) for same square.
%
%==
% ?- between( 1, 64, I ), chess_algebraic_square( Alg, I ), write( I:Alg ), nl, fail.
% 1:a1
% 2:a2
% 3:a3
% 4:a4
% 5:a5
% ...
% 
% ?- findall( Alg, (between(1,64,I),chess_algebraic_square(Alg,I)), Algs ), assert( test_algs(Algs) ).
% Algs = [a1, a2, a3, a4, a5, a6, a7, a8, b1|...].
% 
% ?- test_algs(Algs), member(Alg,Algs), chess_algebraic_square(Alg,Sqr), write(Alg:Sqr), nl, fail.
% a1:1
% a2:2
% a3:3
% a4:4
% ...
% b1:9
% b2:10
% ... 
%==
%
chess_algebraic_square( Alg, Sqr ) :-
    ground( Alg ),
    !,
    atom_codes( Alg, [Lcd|Ncd] ),
    Col is Lcd - 0'a,
    Row is Ncd - 0'0,
    Sqr is Row + (Col * 8).
chess_algebraic_square( Alg, Sqr ) :-
    % Col is J is ((Sqr - 1) // 8) + 1,
    % Row is ((Sqr - 1) mod 8) + 1,
    Col is ((Sqr - 1) // 8),
    Row is ((Sqr - 1) mod 8),
    Ccd is 0'a + Col,
    Rcd is 0'1 + Row,
    atom_codes( Alg, [Ccd,Rcd] ).

