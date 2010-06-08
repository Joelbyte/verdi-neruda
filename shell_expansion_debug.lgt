
:- object(shell_expansion_debug,
	implements(expanding),
	extends(shell_expansion)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is '.']).

	goal_expansion(debug(Goal), Goal).

	term_expansion((Goal & Goals), [Goal|List]) :-
		phrase(::flatten_goals(Goals), List).

:- end_object.  
