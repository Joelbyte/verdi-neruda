
:- object(bfs_interpreter,
	implements(interpreterp)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Breadth-first interpreter for general logic programs.']).

	prove(Goal) :-
		prove(Goal, -1).	

	prove(Goal, Limit) :-
		State = state([Goal], 0, []),
		queue::jump(State, Q0 - Q0, Q),
		prove_branch(Q, Limit).

	prove_branch(Q, _) :-
		queue::head(Q, State), 
		State = state([], _, Bindings), %Goal state.	  
		execute_bindings(Bindings).
	prove_branch(Q, Limit) :-
		queue::serve(Q, State, Q1),
		State = state(Goals, Depth, Bindings),
		0 =\= Depth - Limit, 
		(	Goals = [not(G)|Gs] ->
			(	prove(G) ->
				prove_branch(Q1, Limit) %The whole branch failed. Move on!
			;	Depth1 is Depth + 1,
				State1 = state(Gs, Depth1, Bindings),
				queue::join(State1, Q1, Q2),
				counter::increment, %Inference counting.
				prove_branch(Q2, Limit) %and continue with the rest of the branches.
			)
		;	expand_state(State, NewGoals),
			queue::join_all(NewGoals, Q1, Q2),
			prove_branch(Q2, Limit)
		).

	expand_state(state([], _, _), []) :- !.
	expand_state(state([Goal|Goals], Depth0, Bindings), NewGoals) :-
		Depth is Depth0 + 1,
		%%Find all bodies which unifies with Goal. Since rules are
		%%represented as difference lists it is easy to append the
		%%new body with Goals. Goal in the template is a placeholder,
		%%and is later used in add_bindings/5 to create a unifier
		%%between the old goal and the resolvent.
		bagof(state(Body, Depth, Goal),
			  (
			   rule(Goal, Body, Goals),
			   counter::increment %Inference counting.
			  ),
			NewGoals0),
		!,
		add_bindings(NewGoals0, Goal, Bindings, NewGoals, []).
	expand_state(_, []).

	add_bindings([], _, _, Tail, Tail).
	add_bindings([State0|States0], Goal, Bindings, [State|States], Tail) :-
		State0 = state(Goals, Depth, Goal0),
		State = state(Goals, Depth, [Goal = Goal0|Bindings]),
		add_bindings(States0, Goal, Bindings, States, Tail).

	execute_bindings([]).	
	execute_bindings([X = Y|Bs]) :-
		X = Y,
		execute_bindings(Bs).

	rule(Head, Body, Tail) :-
		(	database::builtin(Head) ->
			call(Head),
			Body = Tail
		;	database::rule(Head, Body, Tail)
		).	

:- end_object.
