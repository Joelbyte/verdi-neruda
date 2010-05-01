
:- object(a_star_interpreter,
          imports(best_first)).

    :- protected(f/4).

    f(Cost0, _Length1, Length2, Cost) :-
        Cost is Length2 + Cost0.

:- end_object.  
