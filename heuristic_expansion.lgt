
:- object(heuristic_expansion(Mode),
	implements(expanding),
	extends(rule_expansion(Mode))).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	%Old definition of builtin expansion, remove later.	
	%term_expansion(builtin(Goal), (rule(Goal, T, 1, T) :- Goal)).
	term_expansion(builtin(Goal), [builtin(Goal), rule(Goal, {Goal}, 1, [])]).

	term_expansion((Head <- Goals), rule(Head, List, Length, Tail)) :-
		phrase(::flatten_goals(Goals), List0),
		list::length(List0, Length),
		list::append(List0, Tail, List).

	term_expansion((:- end_object), [(rule(Head,Body) :- rule(Head,Body, _, _)), (:- end_object)]).

:- end_object.  
