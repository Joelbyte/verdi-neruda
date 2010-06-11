
:- object(bfs_interpreter,
	implements(interpreterp)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Interpreter using a breadth-first search.']).

	prove(Goal) :-
		State = state([Goal], []),
		queue::jump(State, Q0 - Q0, Q),
		prove1(Q).

	prove1(Q) :-
		queue::head(Q, State), 
		State = state([], Bindings), %Goal state.	  
		execute_bindings(Bindings).
	prove1(Q) :-
		queue::serve(Q, State, Q1),
		State = state(Goals, Bindings),
		(	Goals = [not(G)|Gs] ->
			(	prove(G) ->
				prove1(Q1) %The whole branch failed. Move on!
			;	State1 = state(Gs, Bindings),
				queue::join(State1, Q1, Q2),
				counter::increment, %Inference counting.
				prove1(Q2) %and continue with the rest of the branches.
			)
		;	expand_state(State, NewGoals),
			queue::join_all(NewGoals, Q1, Q2),
			prove1(Q2)
		).

	expand_state(state([], _), []) :- !.
	expand_state(state([Goal|Goals], Bindings), NewGoals) :-
		%%Find all bodies which unifies with Goal. Since rules are
		%%represented as difference lists it is easy to append the
		%%new body with Goals. Goal in the template is a placeholder,
		%%and is later used in add_bindings/5 to create a unifier
		%%between the old goal and the resolvent.
		bagof(state(Body, Goal),
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
		State0 = state(Goals, Goal0),
		State = state(Goals, [Goal = Goal0|Bindings]),
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
