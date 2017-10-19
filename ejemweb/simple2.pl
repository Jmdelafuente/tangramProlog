#!/usr/bin/env /home/claudio/tmp/src/ciao-devel/build/bin/ciao-shell
% -*- mode: ciao; -*-

:- use_package(library(pillow)).
	
main(_) :-
	T = [ cgi_reply,
	html( [
		  'Hello ',
		  b(world)
	      ])
	    ],
	output_html(T).



