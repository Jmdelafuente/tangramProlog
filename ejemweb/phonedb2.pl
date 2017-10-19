#!/usr/bin/env /home/claudio/tmp/src/ciao-devel/build/bin/ciao-shell

:- use_package(library(pillow)).
:- use_module(library(write)).

main(_):-
	get_form_input(Input),
	get_form_value(Input,person_name,Name),
	response(Name,Response),
	output_html([
	'Content-type: text/html\n\n',
	html([title('Telphone database'),
	      img$[src="../images/phone.gif",height="42",width="42"],
	      h2('Telephone database'),
	      hr$[],
	      Response])]).

response(Name,Response):-
	form_empty_value(Name) ->
	 Response = 'You have to provide a name.'
	;  phone(Name,Phone) ->
	    Response = ['Telephone number of ',b(Name),': ',Phone]
                ; Response = ['No Telephone number available for ',b(Name),'.'].


phone(daniel, '336-7448').
phone(manuel, '336-7435').
phone(sacha, '543-5316').
