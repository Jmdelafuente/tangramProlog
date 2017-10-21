:- module(hnew,_,_).
:- use_module(library(lists)).
:- use_module(library(system_extra)).

neighbours(o103,[ts,l2d3,o109]).
neighbours(ts,[mail]).
neighbours(mail,[]).
neighbours(o109,[o111,o119]).
neighbours(o111,[]).
neighbours(o119,[storage,o123]).
neighbours(storage,[]).
neighbours(o123,[r123,o125]).
neighbours(o125,[]).
neighbours(l2d1,[l3d2,l2d2]).
neighbours(l2d2,[l2d4]).
neighbours(l2d3,[l2d1,l2d4]).
neighbours(l2d4,[o109]).
neighbours(l3d2,[l3d3,l3d1]).
neighbours(l3d1,[l3d3]).
neighbours(l3d3,[]).
neighbours(r123,[]).


% is_goal(N) is true if N is a goal node.
is_goal([[1,2,2,2,2,2],
         [1,1,2,2,2,3],
	 [1,1,1,2,3,3],
	 [1,1,4,4,3,5],
	 [1,4,4,5,5,5]]).

% cost(N,M,C) is true if C is the arc cost for the arc from node N to node M
cost(N,M,C) :-
   neighbours(N,NN),
   member(M,NN),
   position(N,NX,NY),
   position(M,MX,MY),
   C is abs(NX-MX)+abs(NY-MY).

% N.B. the cost database in the book is obtained by the instances of the query
% ? cost(A,B,C).

% h(N,C) is true if C is the heuristic cost of node N
%  This assumes that there is only one goal node.
h(N,C) :-
   position(N,NX,NY),
   is_goal(G),
   position(G,GX,GY),
   C is abs(NX-GX)+abs(NY-GY).


% position(N,X,Y) is true if node X is at position (X,Y)

position(mail,17,43).
position(ts,23,43).
position(o103,31,43).
position(o109,43,43).
position(o111,47,43).
position(o119,42,58).
position(o123,33,58).
position(o125,29,58).
position(r123,33,62).
position(l2d1,33,49).
position(l2d2,39,49).
position(l2d3,32,46).
position(l2d4,39,46).
position(l3d1,34,55).
position(l3d2,33,52).
position(l3d3,39,52).
position(storage,45,62).


%Predicados para el uso de Matrices
fila(M, N, Row) :-
    nth(N, M, Row).

columna(M, N, Col) :-
    transp(M, MT),
    fila(MT, N, Col).

transp([[]|_], []).
transp([[I|Is]|Rs], [Col|MT]) :-
    primer_columna([[I|Is]|Rs], Col, [Is|NRs]),
    transp([Is|NRs], MT).

primer_columna([], [], []).
primer_columna([[]|_], [], []).
primer_columna([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
    primer_columna(Rs, Col, Rest).

quitarColumna([],[],[]).
quitarColumna([HM|TM],[T|TR],[H|ListaRemovidos]):-
	HM=[H|T],quitarColumna(TM,TR,ListaRemovidos).

quitarNColumnas(Matriz,Matriz,0).
quitarNColumnas(Matriz,NuevaMatriz,N):-
	quitarColumna(Matriz,MatrizA,_L),M is N-1,quitarNColumnas(MatrizA,NuevaMatriz,M).

agregarColumna([],[],[]).
agregarColumna([[H|T]|TM],[Hr|Tr],[[Hr,H|T]|MatrizR]):-
	agregarColumna(TM,Tr,MatrizR).

recortarMatriz([],_N,[]).
recortarMatriz([H|T],N,[NH|MatrizRecortada]):-
	recortarNLista(H,N,NH),recortarMatriz(T,N,MatrizRecortada).

%Predicados para el uso de filas


%Recuperar Lista con las N ultimas posiciones
recortarNLista(L,M,R):-
	length(L,N2),
	N is (N2-M),
	recortarLista(L,N,R).


%Recortar N lugares a la lista
recortarLista(L,0,L).
recortarLista([_H|T],N,R):-
	N2 is N-1,
	recortarLista(T,N2,R).

%Elimina los elementos duplicados de una lista.
sinDuplicados([],[]).
sinDuplicados([X|Xs],Ys):- member(X,Xs), sinDuplicados(Xs,Ys).
sinDuplicados([X|Xs],[X|Ys]):-sinDuplicados(Xs,Ys).

%Figuras o Tans del Juego
listaFiguras(Azul,Rosa,Marron,Rojo,Negro):-
	triangulo1(Azul),triangulo2(Rosa),zeta3(Marron),zeta4(Rojo),ele5(Negro).

triangulo1([[1,_,_],
 	    [1,1,_],
	    [1,1,1],
	    [1,1,_],
	    [1,_,_]]).

triangulo2([[2,2,2,2,2],
	    [_,2,2,2,_],
	    [_,_,2,_,_]]).

zeta3([[_,3],
      [3,3],
      [3,_]]).

zeta4([[_,4,4],
      [4,4,_]]).

ele5([[_,_,5],
	[5,5,5]]).

%Estado inicial
matriz([[_E1,_E2,_E3,_E4,_E5,_E6,_E7,1,_E9,_E10,_E11,_E12,_E13],
	[_D1,_D2,_D3,_D4,_D5,_D6,_D7,1,1,_D10,_D11,_D12,_D13],
	[_C1,3,2,2,2,2,2,1,1,1,_C11,_C12,_C13],
	[3,3,_B3,2,2,2,_B7,1,1,4,4,_B12,5],
	[3,_A2,_A3,_A4,2,_A6,_A7,1,4,4,5,5,5]]).

matrizGround([[0,0,0,0,0,0,0,1,0,0,0,0,0],
	     [0,0,0,0,0,0,0,1,1,0,0,0,0],
	     [0,3,2,2,2,2,2,1,1,1,0,0,0],
	     [3,3,0,2,2,2,0,1,1,4,4,0,5],
	     [3,0,0,0,2,0,0,1,4,4,5,5,5]]).


%Predicados propios del dominio: Manejo de Fichas y recorridos de incersion
%Retorna la cantidad de fichas que no estan o estan fuera de lugar entre dos filas.
fichasDesacomodadasFila(F1,F2,N):-
	comprobarFilas(F1,F2,R),
	length(R,N).

%Comprueba el valor de la heuristica contra el goal, devuelve la cantidad de fichas desacomodadas
comprobar(Matriz,MatrizFinal,Heuristica):-
	comprobarMatriz(Matriz,MatrizFinal,Resultado),
	length(Resultado,Heuristica).

%Buscar fichas desacomodadas, devuelve lista con los numeros de fichas fuera de lugar, no la cantidad.
comprobarMatriz(_M1,[],[]).
comprobarMatriz([H1|T1],[H2|T2],Resultado):-
	comprobarFilas(H1,H2,Rt),comprobarMatriz(T1,T2,R),union(R,Rt,Resultado).

%Comprueba fichas fuera de lugar, F1 es la fila de la matriz actual, F2 la fila de la matriz del estado final.
comprobarFilas(F1,F2,R):-
	length(F2,N),
	%hacerGround(F1,FR1),
	recortarNLista(F1,N,FR),
	diferentes(F2,FR,Res),
	sinDuplicados(Res,R).

%Predicados de Unificacion para variables anonimas y lugares libres
%hacerGround([],[]).
%hacerGround([H1|T1],[H1|R]):-
%	nonvar(H1),hacerGround(T1,R).
%hacerGround([H1|T1],[0|R]):-
%	var(H1),hacerGround(T1,R).


%deshacerGround([],[]).
%deshacerGround([0|T1],[_|R]):-
%	deshacerGround(T1,R).
%deshacerGround([H1|T1],[H1|R]):-
%	deshacerGround(T1,R).


mover_ficha(Estado,[HFicha|TFicha],NuevoEstado):-
	recuperar_numero([HFicha|TFicha],Numero),remover_ficha(Estado,Numero,EstadoIntermedio),length(HFicha,N),quitarNColumnas(EstadoIntermedio,Grilla,N),posicion_valida(Grilla,[HFicha|TFicha],NuevoEstado).

posicion_valida([X|Matriz],Ficha,EstadoNuevo):-X=[0|_T],insertar_ficha([X|Matriz],Ficha,EstadoNuevo).
posicion_valida([X|Fila],Ficha,[X|EstadoNuevo]):- posicion_valida(Fila,Ficha,EstadoNuevo).
posicion_valida([X|Fila],Ficha,EstadoNuevo):- X=[_H|_T],quitarColumna([X|Fila],MatrizR,ListaRemovidos),posicion_valida(MatrizR,Ficha,NuevaMatriz),agregarColumna(NuevaMatriz,ListaRemovidos,EstadoNuevo).


remover_ficha([],_Ficha,[]).
remover_ficha([Fila|Tm],Numero,[NFila|EstadoParcial]):-
	remover_fila(Fila,Numero,NFila),remover_ficha(Tm,Numero,EstadoParcial).

remover_fila([],_Ficha,[]).
remover_fila([Ficha|T],Ficha,[0|NFila]):-remover_fila(T,Ficha,NFila).
remover_fila([H|T],Ficha,[H|NFila]):-remover_fila(T,Ficha,NFila).

recuperar_numero([],0).
recuperar_numero([[HFicha|_T]|_TFicha],HFicha):-nonvar(HFicha).
recuperar_numero([_HFicha|TFicha],Numero):-recuperar_numero(TFicha,Numero).

insertar_ficha([],[],[]).
insertar_ficha([],[_Fila1|_Fi],_):- false.
insertar_ficha([Fila|Mi],[],[Mn|MN]):- insertar_fila(Fila,[],Mn), insertar_ficha(Mi,[],MN).
insertar_ficha([Fila|Mi],[Fila1|Fi],[Mn|MN]):- insertar_fila(Fila,Fila1,Mn), insertar_ficha(Mi,Fi,MN).

insertar_fila([],[],[]).
insertar_fila([X|Lista],[],[X|Mn]):- insertar_fila(Lista,[],Mn).
insertar_fila([0|Lista],[Xf|ListaF],[Xf|Mn]):- insertar_fila(Lista,ListaF,Mn).


%Comprueba de forma ordenada si las listas difieren y retorna los elementos de F2 que no están o están fuera de orden en F1.
diferentes([],[],[]).
diferentes([H1|T1],[H1|T2],R):-
	diferentes(T1,T2,R).
diferentes([_H1|T1],[H2|T2],[H2|R]):-
	diferentes(T1,T2,R).


%PRUEBAS Reducidas
matrizP([[2,_,_],[_,_,_],[_,_,_],[_,_,_]]).

ficha1([[1,_],[1,_],[1,1]]).
ficha2([[1,_],[1,_],[1,1],[1,1]]).

