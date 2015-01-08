:- set_prolog_flag(verbose,silent).
:- prompt(_, '').
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- dynamic macierz_db/2.

macierz(0, [1, 2, 3, 4, 5, 6]).
macierz(1, [[1,2,3],[4,5,6],[7,8,9]]).
macierz(2, [[10,20,30],[40,50,60],[70,80,90]]).
macierz(3, [[[1, 2],[3, 4]],[[5, 6],[7, 8]]]).
macierz(4, [[[10, 20],[30, 40]],[[50, 60],[70, 80]]]).
macierz(5, [[1,0,0],[0,1,0],[0,0,1]]).
macierz(6, [[[1,10],[2,20],[3,30]],[[4,40],[5,50],[6,60]],[[7,70],[8,80],[9,90]]]).
macierz(7, [[[1,2],[3,4]],[[5,6],[7,8]],[[9,10],[11,12]]]).


main:-
	assert_database,
	main_menu(8),
	clear_database,
	halt.

assert_database:-
	macierz(IdMacierzy, Macierz),
	assertz(macierz_db(IdMacierzy, Macierz)),
	fail.
assert_database:- !.

clear_database:-
	retract(macierz_db(_,_)),
	fail.
clear_database :- !.

wyswietl_macierze(X,X):-
	write('\n'),
	write('To ju¿ wszystkie macierze'),!.
wyswietl_macierze(IdMacierzy,MaxNrMatrix):-
	macierz_db(IdMacierzy,Macierz),
	write('Id='),write(IdMacierzy),
	write('; Macierz='),write(Macierz),
	write('\n'),
	NextId is IdMacierzy+1,
	wyswietl_macierze(NextId,MaxNrMatrix).

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

dodaj_macierz(NewID):-
	write('\n'),
	write('Podaj Macierz: '),
	read(Macierz),
	assertz(macierz_db(NewID,Macierz)),
	write('\n'),
	write('Macierz zosta³a dodana'),
	write('\n').
	
podaj_id_macierzy(Id):-
		write('\n'),
		write('Podaj Id macierzy: '), read(Id).

main_menu(MatrixCount):-
	Max_Matrix = MatrixCount,
	writeln('############################################################################'),
	writeln('Baza danych'),
	writeln('1 - wyswietl wszystkie macierze'),
	writeln('2 - dodaj macierz do bazy danych'),
	writeln('3 - Dodaj macierz X + Y'),
	writeln('4 - Mno¿enie macierzy X przez skalar s '),
	writeln('5 - Mno¿enie macierzy X przez macierz Y '),
	writeln('6 - Transpozycja macierzy X'),
	writeln('7 - Zamiana wierszy o indeksach i oraz j w macierzy X'),
	writeln('8 - zamiana wierszy o indeksach [i1,i2] oraz [j1,j2] w macierzy X'),
	writeln('9 - wyjscie z programu'),
	read(Opcja),
	write('wybrano opcje = '), writeln(Opcja),
	( 
		Opcja =:=1  -> menu_option(1, Max_Matrix), NewMatrixCount is Max_Matrix;
		Opcja =:=2  -> menu_option(2, Max_Matrix), NewMatrixCount is Max_Matrix+1;
		Opcja =:=3  -> menu_option(3), NewMatrixCount is Max_Matrix;
		Opcja =:=4  -> menu_option(4), NewMatrixCount is Max_Matrix;
		Opcja =:=5  -> menu_option(5), NewMatrixCount is Max_Matrix;
		Opcja =:=6  -> menu_option(6), NewMatrixCount is Max_Matrix;
		Opcja =:=7  -> menu_option(7), NewMatrixCount is Max_Matrix;
		Opcja =:=8  -> menu_option(8), NewMatrixCount is Max_Matrix;
		Opcja =:=9  -> menu_option(9);
		write('Not implemented')
	),
	nl,
main_menu(NewMatrixCount).

menu_option(1,Max_Matrix):-
	wyswietl_macierze(0,Max_Matrix).
	
menu_option(2, Max_Matrix):-
dodaj_macierz(Max_Matrix).

menu_option(3):-
podaj_id_macierzy(Id),
macierz_db(Id,X),
write('X: '),
write(X),
write('\n'),
podaj_id_macierzy(Id2),
macierz_db(Id2,Y),
write('Y: '),
write(Y),
write('\n'),
write('\n'),
write('Suma macierzy X+Y: '),
add(X,Y,Wynik),
write(Wynik),
write('\n').

menu_option(4):-
podaj_id_macierzy(Id),
macierz_db(Id,X),
write('X: '),
write(X),
write('\n'),write('\n'),
write('Podaj skalar s: '), read(S),
write('s: '),
write(S),
write('\n'),
write('\n'),
write('Iloczyn X*s: '),
scale(X, S, Wynik),
write(Wynik),
write('\n').

menu_option(5):-
podaj_id_macierzy(Id),
macierz_db(Id,X),
write('X: '),
write(X),
write('\n'),
podaj_id_macierzy(Id2),
macierz_db(Id2,Y),
write('Y: '),
write(Y),
write('\n'),
write('\n'),
write('Iloczyn macierzy X*Y: '),
multiply(X, Y, Wynik),
write(Wynik),
write('\n').

menu_option(6):-
podaj_id_macierzy(Id),
macierz_db(Id,X),
write('X: '),
write(X),
write('\n'),write('\n'),
write('X po Transpozycji: '),
transpose(X, Wynik),
write(Wynik),
write('\n').

menu_option(7):-
podaj_id_macierzy(Id),
macierz_db(Id,X),
write('X: '),
write(X),
write('\n'),write('\n'),
write('Podaj indeks wiersza i: '),read(I),
write('\n'),write('\n'),
write('Podaj indeks wiersza j: '),read(J),
write('\n'),write('\n'),
write('Macierz X po zamianie wierszy i z j:'),
swap(X,I,J,Wynik),
write(Wynik),
write('\n').

menu_option(8):-
podaj_id_macierzy(Id),
macierz_db(Id,X),
write('X: '),
write(X),
write('\n'),write('\n'),
write('Podaj indeks (dwuelementowy) wiersza i: '),read(I),
write('\n'),write('\n'),
write('Podaj indeks (dwuelementowy) wiersza j: '),read(J),
write('\n'),write('\n'),
write('Macierz X po zamianie wierszy i z j:'),
swap_advanced(X,I,J,Wynik),
write(Wynik),
write('\n').

menu_option(9):-
	writeln('nacisnij ENTER'),
	halt,
	!.
	
:- main.
