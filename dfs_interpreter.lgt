
:- object(dfs_interpreter,
	implements(interpreterp)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Simple interpreter using a depth-first search.']).

	prove(Goal) :-
		prove_body([Goal]).

	prove_body([]).
    prove_body([Goal|Goals]) :-
        (   Goal = not(G) ->
            (   prove(G) ->
                fail
            ;   prove_body(Goals)
            )
        ;   database::rule(Goal, Body, Goals),
	 	    prove_body(Body)
        ).
            
	% prove_body([not(Goal)|Goals]) :-	% PM: missing a cut here?
	% 	(prove(Goal) -> fail ; prove_body(Goals)).
	% prove_body([Goal|Goals]) :-
	% 	database::rule(Goal, Body, Goals),
	% 	prove_body(Body).

:- end_object.
