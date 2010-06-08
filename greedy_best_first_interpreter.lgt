
:- object(greedy_best_first_interpreter,
	imports(best_first)).

	f(Length1, Length2, _Depth, Cost) :-
		Cost is Length1 + Length2 - 1.

:- end_object.  
