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

read(Opcja),
	write('wybrano opcje = '), writeln(Opcja),
	write('\n').
:- main.
