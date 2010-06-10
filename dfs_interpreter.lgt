
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
			;	counter::increment, %Inference counting.
				prove_body(Goals)
			)
		;	rule(Goal, Body, Goals),
			counter::increment, %Inference counting.
			prove_body(Body)
		).

	rule(Head, Body, Tail) :-
		database::rule(Head, Body0, Tail),
		(	Body0 = {Head} -> 
			call(Head), %Builtin.
			Body = Tail
		;	Body = Body0
		).	

:- end_object.
