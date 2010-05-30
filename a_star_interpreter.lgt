
:- object(a_star_interpreter,
          imports(best_first)).

    :- protected(f/4).

    f(Length1, Length2, Depth, Cost) :-
        Cost is (Length1 + Length2 - 1) + Depth.
:- end_object.  
