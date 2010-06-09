
:- object(a_star_interpreter_weighted,
	imports(best_first)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Weigthed version of A* using w = 1/5.']).

	f(Length1, Length2, Depth, Cost) :-
		Cost is Depth*(1 - 1/5) + (Length1 + Length2 - 1)*(1/5).

:- end_object.  
