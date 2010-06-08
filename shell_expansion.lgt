
:- object(shell_expansion(Mode),
	implements(expanding),
	extends(rule_expansion(Mode))).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is '.']).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Goal & Goals), [Goal|List]) :-
		phrase(::flatten_goals(Goals), List).

:- end_object.  
