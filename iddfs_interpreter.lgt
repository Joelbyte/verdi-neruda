%%TODO: Only allow a new bound if some progress has been made.

:- object(iddfs_interpreter(_Increment),
	implements(interpreterp)).	

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Interpreter using iterative deepening depth-first search. Based on source code from The Craft of Prolog, by Richard O\'Keefe.',
		parnames is ['Increment']]).

	prove(Goal) :-
		parameter(1, Increment),
		prove([Goal], 1, Increment).

	prove(Goals, Bound, Increment) :-
		bounded_prove(Goals, Bound, Remaining),
		Remaining < Increment.
	prove(Goals, Bound, Increment) :-
		Bound1 is Bound + Increment,
		prove(Goals, Bound1, Increment).

	bounded_prove([], Remaining, Remaining).
	bounded_prove([not(Goal)|Goals], Bound, Remaining) :-
		Bound1 is Bound - 1,
		Bound1 >= 0,
%		(bounded_prove([Goal], Bound1, _) -> fail ; bounded_prove(Goals, Bound1, Remaining)).
		(dfs_interpreter::prove(Goal) -> fail ; bounded_prove(Goals, Bound1, Remaining)).
	bounded_prove([Goal|Goals], Bound, Remaining) :-
		Bound1 is Bound - 1,
		Bound1 >= 0,		
		database::rule(Goal, Body, Goals),
		bounded_prove(Body, Bound1, Remaining).

:- end_object.
