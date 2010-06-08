
:- object(a_star_interpreter_weighted2,
	imports(best_first)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is '.']).

	f(Length1, Length2, Depth, Cost) :-
		Cost is Depth*(1 - 99/100) + (Length1 + Length2 - 1)*(99/100).

:- end_object.  
