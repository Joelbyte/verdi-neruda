
:- object(debug_expansion(_Mode),
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/04/15,
		comment is 'Expands debug/1 calls. The parameter Mode can be either the atom "debug" or "production".',
		parnames is ['Mode']]).

	goal_expansion(debug(Goal), ExpandedGoal) :-
		parameter(1, Mode),
		(	Mode == debug ->
			ExpandedGoal = Goal
		;	ExpandedGoal = true
		).

:- end_object.
