#!/usr/bin/env /home/claudio/tmp/src/ciao-devel/build/bin/ciao-shell

:- use_package(library(pillow)).
:- use_module(library(write)).

main(_):-
	output_html([
			cgi_reply,
			start,
			title('Telephone database'),
			heading(2,'Telephone database'),
			$,
			start_form('http://localhost/~claudio/cgi-bin/phonedb3.pl'),
			'Click here, enter name of clip member, and press Return:',
			\\,
			input(text,[name=person_name,size=20]),
			end_form,
			end]).

