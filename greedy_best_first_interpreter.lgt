
:- object(greedy_best_first_interpreter,
          imports(best_first)).

    :- protected(f/4).

    f(_, Length1, Length2, Cost) :-
        Cost is Length1 - 1 + Length2.

:- end_object.  
