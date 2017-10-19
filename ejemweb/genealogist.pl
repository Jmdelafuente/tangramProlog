:- module(genealogist, 
	[  ancestor_decendant/2,
	   siblings/2,
	   parent_child/2,
	   father_child/2,
	   mother_child/2,
	   assert_father_child/2,
	   assert_mother_child/2,	   
	   retract_father_child/2,
	   retract_mother_child/2
	 ]).

:- use_package(persdb).

persistent_dir(genealogist_dir,'./genealogistbase').

:- persistent(mother_child/2,genealogist_dir).
:- persistent(father_child/2,genealogist_dir).

ancestor_decendant(X, Y) :- parent_child(X, Y).
ancestor_decendant(X, Z) :- parent_child(X, Y), ancestor_decendant(Y, Z).


siblings(X, Y) :- parent_child(Z, X), parent_child(Z, Y), X @< Y.

parent_child(X, Y) :- mother_child(X, Y).
parent_child(X, Y) :- father_child(X, Y).

% mother_child(trude, sally).

% father_child(tom, sally).
% father_child(tom, erica).
% father_child(mike, tom).


assert_mother_child(Mother, Child) :-
 	asserta_fact(mother_child(Mother, Child)).

assert_father_child(Father, Child) :-
 	asserta_fact(father_child(Father, Child)).
	
retract_mother_child(Mother, Child) :-
 	retractall_fact(mother_child(Mother, Child)).

retract_father_child(Father, Child) :-
 	retractall_fact(father_child(Father, Child)).

