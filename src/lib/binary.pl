/** binary( +DecInter, -Binary )

Convert between decimal based integers and their binary represenation.

==
?- binary( 16, Sixteen ).
Sixteen = '10000'.

?- binary( 15, Fifteen ).
Fifteen = '1111'.

?- binary( Sixteen, '10000' ).
Sixteen = 16.

?- binary( Fifteen, '1111' ).
Fifteen = 15.
==

From an old general one-directional, base conversion predicate.

@author nicos angelopoulos
@version  0:1 2020/03/26

*/

binary( DecInter, Binary ) :-
    ground( DecInter ),
    !,
    to_binary( DecInter, [0'0,0'1], RegetniCs ),
    reverse( RegetniCs, BiCs ),
    atom_codes( Binary, BiCs ).
binary( DecInter, Binary ) :-
    % to_decimal( DecInter, [0'0,0'1], RegetniCs ),
    ground( Binary ),
    !,
    atom_codes( Binary, RevBinCodes ),
    reverse( RevBinCodes, BinCodes ),
    findall( Summand, (nth1(I,BinCodes,0'1),Summand is 2 ^ (I-1)), Summands ),
    sum_list( Summands, DecInter ).

to_binary( 0, _Digits, Chars ) :-
    !,
    Chars = [].
to_binary( DecInter, Digits, Chars ) :-
    Digit2 is DecInter mod 2,
    NxInt10 is DecInter // 2,
    DigitPos is Digit2 + 1,
    nth1( DigitPos, Digits, Hc ),
    Chars = [Hc|TCs],
    to_binary( NxInt10, Digits, TCs ).
