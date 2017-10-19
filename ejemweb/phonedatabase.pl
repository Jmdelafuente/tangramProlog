:- module(phonedatabase,[response/2,add_phone/2,shutdown/0]).

:- use_module(library('pillow/html')).
:- use_package(persdb).

persistent_dir(phone_dir,'./base').

response(Name,Response):-
	form_empty_value(Name) ->
	 Response = 'You have to provide a name.'
	;  phone(Name,Phone) ->
	    Response = ['Telephone number of ',b(Name),': ',Phone]
                ; Response = ['No Telephone number available for ',b(Name),'.'].


add_phone(Name,Phone):-
	atom_number(APhone,Phone),
	asserta_fact(phone(Name,APhone)).

:- persistent(phone/2, phone_dir).

phone(daniel, '336-7448').
phone(manuel, '336-7435').
phone(sacha, '543-5316').

shutdown :- halt.
