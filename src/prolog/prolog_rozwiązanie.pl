:- set_prolog_flag(verbose,silent).
:- prompt(_, '').
:- use_module(library(readutil)).
:- use_module(library(lists)).

main:-
	process,
	halt.


insert(Val,[H|List],Pos,[H|Res]):-
	Pos > 1,
	!,
	Pos1 is Pos - 1,
	insert(Val,List,Pos1,Res).

insert(Val, List, 1, [Val|List]).

splitList([H|T], Head, Rest) :-
	Head = H,
	Rest = T.

add([], [], Result) :-
	Result = [].

add(A, [], Result) :-
	Result = A.

add(A, B, Result) :-
	(integer(A),
	integer(B) ->
	Result is A + B;
	splitList(A, Ahead, Arest),
	splitList(B, Bhead, Brest),
	add(Ahead, Bhead, ResultHead),
	add(Arest, Brest, ResultRest),
	insert(ResultHead, ResultRest, 1, Result)).

scale([], Factor, Result) :-
	Result = [].

scale(A, Factor, Result) :-
	(integer(A) ->
	Result is A * Factor;
	splitList(A, Ahead, Arest),
	scale(Ahead, Factor, ResultHead),
	scale(Arest, Factor, ResultRest),
	insert(ResultHead, ResultRest, 1, Result)).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% N is the dot product of lists V1 and V2.
dot(V1, V2, N) :-
	maplist(product,V1,V2,P),
	sumlist(P,N).

product(N1,N2,N3) :-
	N3 is N1*N2.

% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
multiply(M1, M2, M3) :-
	transpose(M2, MT),
	maplist(mm_helper(MT), M1, M3).

mm_helper(M2, I1, M3) :-
	maplist(dot(I1), M2, M3).

swap(Matrix,IndexI,IndexJ,Result):-
	IndexI2 is IndexI - 1,
	IndexJ2 is IndexJ - 1,
	nth0(IndexI2,Matrix,E,Res0),
	insert(E,Res0,IndexJ2,Res1),
	nth0(IndexJ2,Res1,E1,Res2),
	insert(E1,Res2,IndexI,Result).

swap_advanced(A,From,To,Result):-
	nth0(0,From,FromX,FromRest),
	nth0(0,FromRest,FromY,FromRest2),
	nth0(0,To,ToX,ToRest),
	nth0(0,ToRest,ToY,ToRest2),
	FromX2 is FromX-1,
	FromY2 is FromY-1,
	ToX2 is ToX-1,
	ToY2 is ToY-1,
	nth0(FromX2,A,A_FromX,Res0),
	nth0(FromY2,A_FromX,A_FromXY,Res1),
	nth0(ToX2,A,A_ToX,Res2),
	nth0(ToY2,A_ToX,A_ToXY,Res3),
	insert(A_FromXY,Res3,ToY,Res4),
	insert(Res4,Res2,ToX,Res5),
	insert(A_ToXY,Res1,FromY,Res6),
	nth0(FromX2,Res5,_,Res7),
	insert(Res6,Res7,FromX,Result).






process:-
	M0 = [1, 2, 3, 4, 5, 6],
	write('M0 = '),
	write(M0),
	write('\n'),
	M1 = [[1,2,3],[4,5,6],[7,8,9]],
	write('M1 = '),
	write(M1),
	write('\n'),
	M2 = [[10,20,30],[40,50,60],[70,80,90]],
	write('M2 = '),
	write(M2),
	write('\n'),
	M3 = [[[1, 2],[3, 4]],[[5, 6],[7, 8]]],
	write('M3 = '),
	write(M3),
	write('\n'),
	M4 = [[[10, 20],[30, 40]],[[50, 60],[70, 80]]],
	write('M4 = '),
	write(M4),
	write('\n'),
	M5 = [[1,0,0],[0,1,0],[0,0,1]],
	write('M5 = '),
	write(M5),
	write('\n'),
	add(M1, M2, M6),
	write('M1 + M2 = '),
	write(M6),
	write('\n'),
	add(M3, M4, M7),
	write('M3 + M4 = '),
	write(M7),
	write('\n'),
	scale(M1, 3, M8),
	write('M1 * 3 = '),
	write(M8),
	write('\n'),
	scale(M3, 2, M9),
	write('M3 * 2 = '),
	write(M9),
	write('\n'),
	multiply(M1, M2, M10),
	write('M1 * M2 = '),
	write(M10),
	write('\n'),
	multiply(M1, M5, M11),
	write('M1 * M5 = '),
	write(M11),
	write('\n'),
	transpose(M1, M12),
	write('transpose(M1) = '),
	write(M12),
		write('\n'),
	transpose(M2, M13),
	write('transpose(M2) = '),
	write(M13),
			write('\n'),
	transpose(M4, M14),
	write('transpose(M4) = '),
	write(M14),
		write('\n'),
		write('swap 2nd and 5th element [1, 2, 3, 4, 5, 6] = '),
		swap([1, 2, 3, 4, 5, 6],2,5,Result3),
	write(Result3),
		write('\n'),
		write('swap 1nd and 3rd element [[1,2,3],[4,5,6],[7,8,9]] = '),
		swap([[1,2,3],[4,5,6],[7,8,9]],1,3,Result4),
	write(Result4),
			write('\n'),
		write('swap 1nd and 2nd element [[[1,10],[2,20],[3,30]],[[4,40],[5,50],[6,60]],[[7,70],[8,80],[9,90]]] = '),
		swap([[[1,10],[2,20],[3,30]],[[4,40],[5,50],[6,60]],[[7,70],[8,80],[9,90]]],1,2,Result5),
	write(Result5),

	write('\n'),
	write('\n'),
	write('SWAP ADVANCED'),
	write('\n'),
	swap_advanced([[[1,2],[3,4]],[[5,6],[7,8]],[[9,10],[11,12]]],[1,2],[2,1],Result),
	write('\n'),
	write('Zamiana w macierzy:'),
	write('\n'),
	write('[[ 1, 2],[ 3, 4]]'),
	write('\n'),
	write('[[ 5, 6],[ 7, 8]]'),
	write('\n'),
	write('[[ 9,10],[11,12]]'),
	write('\n'),
	write('elementów [3,4] z [5,6]'),
	write('\n'),
	write(Result),
	write('\n'),
	write('\n'),
	swap_advanced([[1,2,3],[4,5,6],[7,8,9]],[2,2],[3,1],Result2),
	write('\n'),
	write('Zamiana w macierzy:'),
	write('\n'),
	write('[1, 2, 3][4, 5, 6][7, 8, 9]'),
	write('\n'),
	write('element [5] z [7]'),
	write('\n'),
	write(Result2),
	write('\n'),
	swap_advanced([[[1,11,111],[2,22,222],[3,33,333]],[[4,44,444],[5,55,555],[6,66,666]],[[7,77,777],[8,88,888],[9,99,999]]],[1,1],[3,3],Result444),
	write('\n'),
	write('Zamiana w macierzy:'),
	write('\n'),
	write('[[1,11,111],[2,22,222],[3,333,333]]'),
	write('\n'),
	write('[[4,44,444],[5,55,555],[6,66,666]]'),
		write('\n'),
	write('[[7,77,777],[8,88,888],[9,99,999]]'),
	write('\n'),
	write('element [1,11,111] z [9,99,999]'),
	write('\n'),
	write(Result444),
	write('\n'),
read(Opcja),
	write('wybrano opcje = '), writeln(Opcja),
	write('\n').
:- main.
