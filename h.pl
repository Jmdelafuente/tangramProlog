:- module(h,_,_).
:- use_module(library(lists)).


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
is_goal(r123).

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

%Contar
%list_member_occ([], _, 0).       % list is empty, 0 occurrences
%list_member_occ([X|Xs], X, N) :- % list has the element at the head
    list_member_occ(Xs, X, N0),  % count number of elements in the tail
    succ(N0, N).                 % the number of occurrences is the
                                 % next natural number
list_member_occ([Y|Xs], X, N) :-
    X \= Y,                   % head and the element are different
    list_member_occ(Xs, X, N).   % occurrences in the tail of the list
                                 % is the total number

%Matrices
fila(M, N, Row) :-
    nth(M, N, Row).

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

%%  add_matrices(+Matrix, +Matrix, ?Matrix) is det
%
%   Adds given matrices if their dimensions are equal.
%   Uses add_vectors/3 to perform addition on each row.

add_matrices([], [], []).
add_matrices([A|As], [B|Bs], [R|Rs]) :-
    add_vectors(A, B, R),
    add_matrices(As, Bs, Rs).

%   add_vectors(+List, +List, ?List) is det

add_vectors([], [], []).
add_vectors([A|As], [B|Bs], [R|Rs]) :-
    R is A + B,
    add_vectors(As, Bs, Rs).

%figuras

triangulo1([[1,_,_],
	[1,1,_],
	 [1,1,1],
	  [1,1,_],
	   [1,_,_]]).

triangulo2([[2,2,2,2,2],
	[_,2,2,2,_],
	 [_,_,2,_,_]]).

zeta3([_,3],
	[3,3],
	 [3,_]).

zeta4([_,4,4],
	[4,4,_]).

ele5([_,_,5],
	[5,5,5]).



%matriz([[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13],[b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13],[c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13],[c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13],[d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13]]).

%Probar con Matriz(M),Fila(N,M,Fila).

%Matriz inicial
matriz([[_E1,_E2,_E3,_E4,_E5,_E6,_E7,1,_E9,_E10,_E11,_E12,_E13],
	[_D1,_D2,_D3,_D4,_D5,_D6,_D7,1,1,_D10,_D11,_D12,_D13],
	 [_C1,3,2,2,2,2,2,1,1,1,_C11,_C12,_C13],
	  [3,3,_B3,2,2,2,_B7,3,3,4,4,_B12,5],
	   [3,_A2,_A3,_A4,2,_A6,_A7,1,4,4,5,5,5]]).

