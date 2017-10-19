#!/usr/bin/env /home/claudio/tmp/src/ciao-devel/build/bin/ciao-shell

:- use_package(actmods).
:- use_module(library(actmods/filebased_locate)).
:- use_active_module(phonedatabase,[response/2,add_phone/2,shutdown/0]).

:- use_package(library(pillow)).
:- use_module(library(file_utils)).


main(_):-
	get_form_input(Input),
	get_form_value(Input,person_name,Name),
	get_form_value(Input,addname,NewUser),
	get_form_value(Input,addphone,Phone),
	((form_empty_value(NewUser);form_empty_value(Phone)) -> true ; catch(add_phone(NewUser,Phone),_,shutdown)),
	catch(response(Name,Response),_,shutdown),
	file_to_string('TlfDB.html',Contents),
	html_template(Contents, HTML_terms, Dict),
	member(response = Response,Dict),
	output_html([cgi_reply|HTML_terms]).

