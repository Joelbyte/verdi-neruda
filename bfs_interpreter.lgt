
:- object(bfs_interpreter,
          implements(interpreterp)).

    :- info([
        version is 0.1,
        author is 'Victor Lagerkvist',
        date is 2010/03/18,
        comment is 'Interpreter using a breadth-first search.']).

    prove(Goal) :-
        State = state([Goal], []),
        prove1([State|X], X).

    %%Version which uses append instead.    
    prove_clean(Goal) :-
        State = state([Goal], []),
        prove2([State]).

    prove1(Tail, Tail2) :-
        %%This is a terrible but moderately efficient way to
        %%stop the queue from "hallucinating" new elements. A
        %%cleaner solution would be to augment prove1/2 with a
        %%argument which keeps track of the length of the queue.
        (Tail == Tail2 -> !, fail ; true),
        Tail = [state([], Bindings)|Tail1],
        (Tail1 == Tail2 -> ! ; true),
        execute_bindings(Bindings).
    prove1([state([not(Goal)|Goals], Bindings)|Tail1], Tail2) :-
        (prove(Goal) ->
           fail
         ; State = state(Goals, Bindings),
           Tail2 = [State|Tail],
           prove1(Tail1, Tail)
        ).
    prove1([Goal|Goals], Tail1) :-
        expand_goal1(Goal, Tail1, Tail),
        %write('Expanded goals are: '), writeln(Goals),
        prove1(Goals, Tail).

    prove2([state([], Bindings)|_]) :-
        execute_bindings(Bindings).
    prove2([Goal|Goals]) :-
        expand_goal1(Goal, NewGoals, []),
        append(Goals, NewGoals, Goals1),
        prove2(Goals1).

    expand_goal1(state([], _), Tail, Tail) :- !.
    expand_goal1(state([Goal|Goals], Bindings), NewGoals, Tail) :-
        %%Find all bodies which unifies with Goal. Since rules are
        %%represented as difference lists it is easy to append the
        %%new body with Goals. Goal in the template is a placeholder,
        %%and is later used in add_bindings/5 to create a unifier
        %%between the old goal and the resolvent.
        findall(state(Body, Goal),
            database::rule(Goal, Body, Goals),
            NewGoals0),
        !,
        add_bindings(NewGoals0, Goal, Bindings, NewGoals, Tail).
    expand_goal1(_, Tail, Tail).

    add_bindings([], _, _, Tail, Tail).
    add_bindings([State0|States0], Goal, Bindings, [State|States], Tail) :-
        State0 = state(Goals, Goal0),
        State = state(Goals, [Goal = Goal0|Bindings]),
        add_bindings(States0, Goal, Bindings, States, Tail).

    execute_bindings([]).    
    execute_bindings([X = Y|Bs]) :-
        X = Y,
        execute_bindings(Bs).
:- end_object.
