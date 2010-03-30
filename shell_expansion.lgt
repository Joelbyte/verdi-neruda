:- object(shell_expansion,
          extends(rule_expansion),
          implements(expanding)).

    term_expansion((Goal & Goals), [Goal|List]) :-
        ::flatten_goals(Goals, List, []).

:- end_object.  