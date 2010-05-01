
:- category(best_first,
            implements(interpreterp)).

    prove(Goal) :-
        minheap::as_heap([1 - state([Goal], 1, [])], Heap),
        prove_branch(Heap).
    
    prove_branch(Heap) :-
        minheap::top(Heap, _, state([], _, Bindings)),
        execute_bindings(Bindings).
    prove_branch(Heap) :-
        minheap::delete(Heap, Cost, State, Heap1),
        (   State = state([not(G)|Gs], Length, Bindings) ->
                (   prove(G) -> 
                        prove_branch(Heap1)
                    ;
                        Length1 is Length - 1,
                        Cost1 is Cost - 1,
                        minheap::insert(Cost1, state(Gs, Length1, Bindings), Heap1, Heap2),
                        prove_branch(Heap2)
                )
            ;
%                write('New state is: '), writeln(Cost - State),    
                expand_state(Cost, State, StateCostPairs),
%                write('New pairs are: '), writeln(StateCostPairs),
                minheap::insert_all(StateCostPairs, Heap1, Heap2),
                prove_branch(Heap2)
        ).

    expand_state(_, state([], 0, _), []) :- !.
    expand_state(Cost0, state([Goal|Goals], Length1, Bindings), Pairs) :-
        bagof(Cost - state(Body, Cost, Goal),
              Length2^(
                database::rule(Goal, Body, Length2, Goals),
                this(Caller),
                Caller::f(Cost0, Length1, Length2, Cost)
              ),
            NewPairs0),
        !,
        add_bindings(NewPairs0, Goal, Bindings, Pairs).
    expand_state(_, _, []).

    add_bindings([], _, _, []).
    add_bindings([Cost - State0|States0], Goal, Bindings, [Cost - State|States]) :-
        State0 = state(Goals, Length, Goal0),
        State = state(Goals, Length, [Goal = Goal0|Bindings]),
        add_bindings(States0, Goal, Bindings, States).

    execute_bindings([]).    
    execute_bindings([X = Y|Bs]) :-
        X = Y,
        execute_bindings(Bs).
:- end_category.
