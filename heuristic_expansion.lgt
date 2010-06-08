
:- object(heuristic_expansion(Mode),
	implements(expanding),
	extends(rule_expansion(Mode))).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion(builtin(Goal), (rule(Goal, T, 1, T) :- Goal)).

	term_expansion((Head <- Goals), rule(Head, List, Length, Tail)) :-
		phrase(::flatten_goals(Goals), List0),
		list::length(List0, Length),
		list::append(List0, Tail, List).

:- end_object.  
