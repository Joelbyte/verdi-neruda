
:- object(shell_expansion,
	extends(rule_expansion),
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is '.']).

	goal_expansion(debug(_), true).

	term_expansion((Goal & Goals), [Goal|List]) :-
		phrase(::flatten_goals(Goals), List).

:- end_object.  
