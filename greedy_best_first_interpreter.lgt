
:- object(greedy_best_first_interpreter,
          imports(best_first)).

    :- protected(f/4).

    f(Length1, Length2, _Depth, Cost) :-
        Cost is Length1 + Length2 - 1.

:- end_object.  
