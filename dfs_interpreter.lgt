
:- object(dfs_interpreter,
	implements(interpreterp)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Depth-first interpreter for general logic programs.']).

	prove(Goal) :-
		prove_body([Goal], -1).

	prove(Goal, Limit) :-
		prove_body([Goal], Limit).

	prove_body([], _).
	prove_body([Goal|Goals], Limit) :-
		Limit \= 0,
		Limit0 is Limit - 1,
		(	Goal = not(G) ->
			(	prove(G) ->
				fail
			;	counter::increment, %Inference counting.
				prove_body(Goals, Limit0)
			)
		;	rule(Goal, Body, Goals),
			counter::increment, %Inference counting.
			prove_body(Body, Limit0)
		).

	rule(Head, Body, Tail) :-
		(	database::builtin(Head) ->
			call(Head),
			Body = Tail
		;	database::rule(Head, Body, Tail)
		).	

:- end_object.
