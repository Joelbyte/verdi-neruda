
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
		(	Goal = not(G) ->
			(	prove(G) ->
				fail
			;	prove_body(Goals)
			)
		;	rule(Goal, Body, Goals),
			prove_body(Body)
		).

	rule(Head, T, T) :-
		database::rule(Head, {Head}, []), !,
		call(Head).
	rule(Head, Body, Tail) :-
		database::rule(Head, Body, Tail).

:- end_object.
