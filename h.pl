:- module(h,_,_).
:- use_module(library(lists)).
:- use_module(library(system_extra)).
:- use_package(hiord).

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

%Figuras o Tans del Juego para posicion generica
listaFiguras(azul,rosa,marron,rojo,negro).
	%triangulo1(Azul),triangulo2(Rosa),zeta3(Marron),zeta4(Rojo),ele5(Negro).

triangulo1([[1],
 	    [1,1],
	    [1,1,1],
	    [1,1],
	    [1]]).

triangulo2([[2,2,2,2,2],
	    [_,2,2,2],
	    [_,_,2]]).

zeta3([[_,3],
       [3,3],
       [3]]).

zeta4([[_,4,4],
       [4,4]]).

ele5([[_,_,5],
          [5,5,5]]).

%insertar figuras para posicion especifica
azul(N,[H1,H2,H3,H4,H5|To],[Hn1,Hn2,Hn3,Hn4,Hn5|To]):-
	insertarFicha(H1,N,1,1,Hn1),
	insertarFicha(H2,N,2,1,Hn2),
	insertarFicha(H3,N,3,1,Hn3),
	insertarFicha(H4,N,2,1,Hn4),
	insertarFicha(H5,N,1,1,Hn5).

negro(N,[H1,H2|To],[Hn1,Hn2|To]):-
	N1 is N+2,insertarFicha(H1,N1,1,5,Hn1),
	insertarFicha(H2,N,3,5,Hn2).

marron(N,[H1,H2,H3|To],[Hn1,Hn2,Hn3|To]):-
	N1 is N+1,insertarFicha(H1,N1,1,3,Hn1),
	insertarFicha(H2,N,2,3,Hn2),
	insertarFicha(H3,N,1,3,Hn3).

rosa(N,[H1,H2,H3|To],[Hn1,Hn2,Hn3|To]):-
  insertarFicha(H1,N,5,2,Hn1),
  N1 is N+1, insertarFicha(H2,N1,3,2,Hn2),
  N2 is N+2, insertarFicha(H3,N2,1,2,Hn3).
  
rojo(N,[H1,H2|To],[Hn1,Hn2|To]):-
  N1 is N+1, insertarFicha(H1,N1,2,4,Hn1),
  insertarFicha(H2,N,2,4,Hn2).

%insertarFicha(_,-1,_,_,_):-!.
insertarFicha([],0,0,_F,[]).
insertarFicha([H|T],0,0,_F,[H|T]).
insertarFicha([0|T],0,M,F,[F|Tn]):-
	M1 is M-1,insertarFicha(T,0,M1,F,Tn).
insertarFicha([],_,_,_,_):-false.
insertarFicha([H|T],N,M,F,[H|Tn]):-
	N1 is N-1, N1>=0,insertarFicha(T,N1,M,F,Tn).


 %Estado inicial

matriz([[0,0,0,0,0,0,0,1,0,0,0,0,0],
	[0,0,0,0,0,0,0,1,1,0,0,0,0],
	[0,3,2,2,2,2,2,1,1,1,0,0,0],
	[3,3,0,2,2,2,0,1,1,4,4,0,5],
	[3,0,0,0,2,0,0,1,4,4,5,5,5]]).


matrizGround([[0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
	      [0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0],
	      [0,3,2,2,2,2,2,1,1,1,0,0,0,0,0,0],
	      [3,3,0,2,2,2,0,1,1,0,0,0,4,4,0,5],
	      [3,0,0,0,2,0,0,1,0,0,0,4,4,5,5,5]]).


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


%mover_ficha(Estado,[HFicha|TFicha],NuevoEstado):-
%	recuperar_numero([HFicha|TFicha],Numero),remover_ficha(Estado,Numero,EstadoIntermedio),length(HFicha,N),quitarNColumnas(EstadoIntermedio,Gri%lla,N),posicion_valida(Grilla,[HFicha|TFicha],NuevoEstado).

mover_ficha(Estado,Ficha,NuevoEstado):-
	recuperar_numero(Ficha,Numero,Long),remover_ficha(Estado,Numero,EstadoIntermedio),quitarNColumnas(EstadoIntermedio,[Hg|Tg],Long),length(Hg,Tope),N is Tope-6,posicion_valida(N,Tope,[Hg|Tg],Ficha,NuevoEstado).

%Ajuste de eficiencia
%posicion_valida(Tope,Tope,_,_,_):-false.
posicion_valida(N,Tope,[H|Matriz],Ficha,EstadoNuevo):-fila_valida(N,Tope,[H|Matriz],Ficha,EstadoNuevo).
%posicion_valida(N,Tope,Matriz,Ficha,EstadoNuevo):- columna_valida(N,Tope,Matriz,Ficha,EstadoNuevo).
posicion_valida(N,Tope,Matriz,Ficha,EstadoNuevo):- N1 is N+1,Tope>=N1,posicion_valida(N1,Tope,Matriz,Ficha,EstadoNuevo).
%posicion_valida(N,Tope,[X|Fila],Ficha,[X|EstadoNuevo]):- posicion_valida(N,Tope,Fila,Ficha,EstadoNuevo).

fila_valida(_,_,[],_,_):-false.
fila_valida(N,_Tope,[H|Matriz],Ficha,EstadoNuevo):- call(Ficha,N,[H|Matriz],EstadoNuevo).
fila_valida(N,Tope,[X|Fila],Ficha,[X|EstadoNuevo]):- fila_valida(N,Tope,Fila,Ficha,EstadoNuevo).


remover_ficha([],_Ficha,[]).
remover_ficha([Fila|Tm],Numero,[NFila|EstadoParcial]):-
	remover_fila(Fila,Numero,NFila),remover_ficha(Tm,Numero,EstadoParcial).

remover_fila([],_Ficha,[]).
remover_fila([Ficha|T],Ficha,[0|NFila]):-remover_fila(T,Ficha,NFila).
remover_fila([H|T],Ficha,[H|NFila]):-remover_fila(T,Ficha,NFila).

recuperar_numero(azul,1,3).
recuperar_numero(rosa,2,5).
recuperar_numero(marron,3,2).
recuperar_numero(rojo,4,3).
recuperar_numero(negro,5,3).

%recuperar_numero([],0).
%recuperar_numero([[HFicha|_T]|_TFicha],HFicha):-nonvar(HFicha).
%recuperar_numero([_HFicha|TFicha],Numero):-recuperar_numero(TFicha,Numero).

%insertar_ficha([],[],[]).
%insertar_ficha([],[_Fila1|_Fi],_):- false.
%insertar_ficha([Fila|Mi],[],[Mn|MN]):- insertar_fila(Fila,[],Mn), insertar_ficha(Mi,[],MN).
%insertar_ficha([Fila|Mi],[Fila1|Fi],[Mn|MN]):- insertar_fila(Fila,Fila1,Mn), insertar_ficha(Mi,Fi,MN).

%insertar_fila([],[],[]).
%insertar_fila([X|Lista],[],[X|Mn]):- insertar_fila(Lista,[],Mn).
%insertar_fila([0|Lista],[Xf|ListaF],[Xf|Mn]):- insertar_fila(Lista,ListaF,Mn).


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

matriz6([[2,0,0,0,0,0],[2,2,0,0,0,0],[2,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]).