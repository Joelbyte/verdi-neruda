
:- object(heuristic_expansion_debug,
	implements(expanding),
	extends(heuristic_expansion)).

	goal_expansion(debug(Goal), Goal).

	term_expansion(Term, Expansion) :-
		^^term_expansion(Term, Expansion).

:- end_object.  
