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

%matriz([[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13],[b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13],[c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13],[c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13],[d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13]]).

%Probar con Matriz(M),Fila(N,M,Fila).

matriz([[1,a2,a3,a4,2,a6,a7,3,4,4,5,5,5],[1,1,b3,2,2,2,b7,3,3,4,4,b12,5],[c1,1,2,2,2,2,2,3,3,3,c11,c12,c13],[c1,c2,c3,c4,c5,c6,c7,3,3,c10,c11,c12,c13],[d1,d2,d3,d4,d5,d6,d7,3,d9,d10,d11,d12,d13]]).
