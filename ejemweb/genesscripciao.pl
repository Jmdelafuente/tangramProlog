#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-


% #!/usr/bin/env /home/claudio/tmp/src/ciao-devel/build/bin/ciao-shell

:- use_package(actmods).
:- use_module(library(actmods/filebased_locate)).
:- use_active_module(genealogist,[
	                ancestor_decendant/2,
			siblings/2,
			parent_child/2,
			father_child/2,
			mother_child/2,
			assert_father_child/2,
			assert_mother_child/2,	   
			retract_father_child/2,
			retract_mother_child/2
				 ]).

:- use_package(library(pillow)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).

main(_):-
	get_form_input(Input),
	get_form_value(Input,query,Query),
	get_form_value(Input,parent,Parent),
	get_form_value(Input,child,Child),
	get_form_value(Input,sex,Sex),
	get_form_value(Input,action,Action),
	get_form_value(Input,numsol,NumSol),
	(
	    (form_empty_value(Parent);form_empty_value(Child)) -> true
	;
	    (
		Sex = 'father_child' ->
		(
		    Action = assert -> assert_father_child(Parent,Child)
		;
		    retract_father_child(Parent,Child)
		)
	    ;
		(
		    Action = assert -> assert_mother_child(Parent,Child)
		;
		    retract_mother_child(Parent,Child)
		)
	    )
	), 
	(
	    form_empty_value(Query) ->  Output = ' ', NQuery = ' ',NNumSol = 1
	;
	    callquery(Query,NumSol,Output,NNumSol) , NQuery = Query
	),
	file_to_string('templgeneciao.html',Contents),
	html_template(Contents, HTML_terms, Dict),
	member(output = Output,Dict),
	member(queryfield = NQuery,Dict),
	member(numfield = NNumSol,Dict),
	output_html([cgi_reply|HTML_terms]).

callquery(AQuery,NumSol,Output,NNumSol):-
	name(AQuery,SQuery),
	dlist(FSQuery,SQuery,[0'(|REST]),
	dlist(First,REST,[0',|Last]),
	dlist(Second,Last,")"),
	name(FQuery,FSQuery),
	functor(Query,FQuery,2),
        First = [CodeF|_],
	Second = [CodeS|_],
	( code_class(CodeF,2) -> name(VarX,First) ; name(X,First)),
	( code_class(CodeS,2) -> name(VarY,Second) ; name(Y,Second)),
	  arg(1,Query,X),
	  arg(2,Query,Y),
	  findall((X,Y),Query,L),
	  length(L,NL),
	  (
	      NumSol = 0 , outputlist(L,VarX,VarY,Output,[]), NNumSol = 1
	  ;
	      NumSol > NL , outputlist(L,VarX,VarY,Output,['No more solutions']), NNumSol = 1
	  ;
	      length(M,NumSol),dlist(M,L,_), outputlist(M,VarX,VarY,Output,[]), NNumSol is NumSol + 1
	  ).

outputlist([],_,_,L,L).
outputlist([(X,Y)|L],VarX,VarY,Out,Tail):-
        (
	    (nonvar(VarX),nonvar(VarY)) -> Out = [ b(VarX),' = ',X,' and ',b(VarY),' = ',Y,\\ | Output]
	;
	    (nonvar(VarX) -> Out = [ b(VarX),' = ',X,\\ | Output]
	    ;
		(nonvar(VarY) -> Out = [ b(VarY),' = ',Y,\\ | Output]
		;
		    Out = ['true',\\ | Output]
		)
	    )
	),
	outputlist(L,VarX,VarY,Output,Tail).



