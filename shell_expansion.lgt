
:- object(shell_expansion,
	extends(rule_expansion),
	implements(expanding)).

	goal_expansion(debug(_), true).

	term_expansion((Goal & Goals), [Goal|List]) :-
		phrase(::flatten_goals(Goals), List).

:- end_object.  
