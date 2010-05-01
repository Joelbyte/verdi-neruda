
:- object(heuristic_expansion,
          extends(rule_expansion),
          implements(expanding)).

    term_expansion(builtin(Goal), (rule(Goal, T, 1, T) :- Goal)).

    term_expansion((Head <- Goals), rule(Head, List, Length, Tail)) :-
        ::flatten_goals(Goals, List0, []),
        length(List0, Length),
        append(List0, Tail, List).

:- end_object.  
