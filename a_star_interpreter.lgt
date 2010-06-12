
:- object(a_star_interpreter(_W),
	imports(best_first)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'A* interpreter for general logic programs. The parameter W is used to fine tune the behaviour. W = 0 gives us a breadth-first search and W = 1 gives us a greedy best-first search.',
		parnames is ['W']]).

	f(Length1, Length2, Depth, Cost) :-
		parameter(1, W),
		Cost is Depth*(1 - W) + (Length1 + Length2 - 1)*(W).	   

:- end_object.  
