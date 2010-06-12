%%TODO: Only allow a new bound if some progress has been made.

:- object(iddfs_interpreter(_Increment),
	implements(interpreterp)).	

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Iterative deepening depth-first interpreter for general logic programs. Based on source code from The Craft of Prolog, by Richard O\'Keefe.',
		parnames is ['Increment']]).

	prove(Goal) :-
		prove(Goal, -1).		

	prove(Goal, Limit) :-
		parameter(1, Increment),
		prove([Goal], 1, Increment, Limit).

	prove(Goals, Bound, Increment, Limit) :-
		Limit \= 0,
		bounded_prove(Goals, Bound, Remaining),
		Remaining < Increment.
	prove(Goals, Bound, Increment, Limit) :-
		Limit \= 0,
		Limit0 is Limit - 1,		 
		Bound1 is Bound + Increment,
		prove(Goals, Bound1, Increment, Limit0).

	bounded_prove([], Remaining, Remaining).
	bounded_prove([not(Goal)|Goals], Bound, Remaining) :-
		%%TODO::Rewrite as an if-then-else instead?
		!, 
		Bound1 is Bound - 1,
		Bound1 >= 0,
		%%This is a temporary workaround that allows iddfs to handle negation.
		(	dfs_interpreter::prove(Goal) -> 
			fail 
		; 	counter::increment, %Inference counting.
			bounded_prove(Goals, Bound1, Remaining)
		).
	bounded_prove([Goal|Goals], Bound, Remaining) :-
		Bound1 is Bound - 1,
		Bound1 >= 0,		
		rule(Goal, Body, Goals),
		counter::increment, %Inference counting.
		bounded_prove(Body, Bound1, Remaining).

	rule(Head, Body, Tail) :-
		(	database::builtin(Head) ->
			call(Head),
			Body = Tail
		;	database::rule(Head, Body, Tail)
		).	
 
:- end_object.
