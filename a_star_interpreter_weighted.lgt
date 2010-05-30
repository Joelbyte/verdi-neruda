
:- object(a_star_interpreter_weighted,
          imports(best_first)).

    :- protected(f/4).

    f(Length1, Length2, Depth, Cost) :-
            Cost is Depth*(1 - 1/5) + (Length1 + Length2 - 1)*(1/5).

:- end_object.  
