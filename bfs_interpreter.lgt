
:- object(bfs_interpreter,
		    implements(interpreterp)).	

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Interpreter using a breadth-first search.']).

	prove(Goal) :-
	    database::rule(Goal, Body),
	    prove_body(Body).

    prove_body([]).
    prove_body([Goal|Goals]) :-
	    database::rule(Goal, Body),
	    prove_body(Body),
	    prove_body(Goals).

:- end_object.
