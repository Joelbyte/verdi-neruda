
:- category(dfs_interpreter,
		    implements(interpreterp)).	

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Simple interpreter using a depth-first search.']).

	prove(Goal) :-
	    rule(Goal, Body),
	    prove_body(Body).

    prove_body([]).
    prove_body([Goal|Goals]) :-
	    rule(Goal, Body),
	    prove_body(Body),
	    prove_body(Goals).

:- end_category.
