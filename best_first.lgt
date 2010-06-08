
:- category(best_first,
	implements(interpreterp)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Interpreter using iterative deepening depth-first search.']).

	:- protected(f/4).

	prove(Goal) :-
		minheap::as_heap([1 - state([Goal], 1, 0, [])], Heap),
		prove_branch(Heap).
	
	prove_branch(Heap) :-
		minheap::top(Heap, _, state([], _, _, Bindings)),
		execute_bindings(Bindings).
	prove_branch(Heap) :-
		minheap::delete(Heap, Cost, State, Heap1),
		(   State = state([not(G)|Gs], Length, Depth, Bindings) ->
			(   prove(G) -> 
				prove_branch(Heap1)
			;	Length1 is Length - 1,
				Depth1 is Depth + 1,
				this(Caller),
				Caller::f(Length, 0, Depth1, Cost1),
				minheap::insert(Cost1, state(Gs, Length1, Depth1, Bindings), Heap1, Heap2),
				prove_branch(Heap2)
			)
		;	expand_state(Cost, State, StateCostPairs),
			minheap::insert_all(StateCostPairs, Heap1, Heap2),
			prove_branch(Heap2)
		).

	expand_state(_, state([], 0, _, _), []) :- !.
	expand_state(_Cost0, state([Goal|Goals], Length1, Depth0, Bindings), Pairs) :-
		Depth is Depth0 + 1,
		bagof(Cost - state(Body, Length, Depth, Goal),
			  Depth0^Length1^Length2^(
				database::rule(Goal, Body, Length2, Goals),
				Length is Length1 + Length2 - 1,
				this(Caller),
				Caller::f(Length1, Length2, Depth, Cost)
			  ),
			NewPairs0),
		!,
		add_bindings(NewPairs0, Goal, Bindings, Pairs).
	expand_state(_, _, []).

	add_bindings([], _, _, []).
	add_bindings([Cost - State0|States0], Goal, Bindings, [Cost - State|States]) :-
		State0 = state(Goals, Length, Depth, Goal0),
		State = state(Goals, Length, Depth, [Goal = Goal0|Bindings]),
		add_bindings(States0, Goal, Bindings, States).

	execute_bindings([]).
	execute_bindings([X = Y|Bs]) :-
		X = Y,
		execute_bindings(Bs).

:- end_category.
