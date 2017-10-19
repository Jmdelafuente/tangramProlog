#!/usr/bin/env /home/claudio/tmp/src/ciao-devel/build/bin/ciao-shell

:- use_package(library(pillow)).
:- use_module(library(write)).

main(_):-
	get_form_input(Input),
	get_form_value(Input,person_name,Name),
	write('Content-type: text/html'),nl,nl,
	write('<HTML><TITLE>Telphone database</TITLE>'),nl,
	write('<IMG SRC="../images/phone.gif" >'),
	write('<H2>Telephone database</H2><HR>'),
	write_info(Name),
	write('</HTML>').

write_info(Name):-
	form_empty_value(Name) ->
	 write('You have to provide a name.')
	;  phone(Name,Phone) ->
	    write('Telephone number of <B>'),
	    write(Name),
	    write('</B>: '),
	    write(Phone)
		;
		    write('No Telephone number available for <B>'),
		    write(Name),
		    write('</B>.').

phone(daniel, '336-7448').
phone(manuel, '336-7435').
phone(sacha, '543-5316').
