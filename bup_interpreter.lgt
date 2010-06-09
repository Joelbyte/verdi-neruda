
:- object(bup_interpreter,
	implements(interpreterp)).

	:- info([
		version is 0.1,
		author is 'Ulf Nilsson. Ported to Logtalk and augmented with negation by Victor Lagerkvist.',
		date is 2010/04/14,
		comment is 'Semi-naive bottom-up interpreter. Magic transformation is realized through an expansion hook.']).

	prove(Goal) :-
		%%TODO: Does not work with negated goals!
		magic::magic(Goal, MagicGoal),
		prove(Goal, [MagicGoal], [MagicGoal], FixPoint).

	prove(Goal, I, DI, FixPoint) :-
		subsumption_iterate(Goal, I, DI, [], Pending, FixPoint0),
		(   Pending = [] ->
			FixPoint = FixPoint0
		;	satisfy_negative_literals(Pending, FixPoint0, Satisfied),
			subsumption_union(FixPoint0, Satisfied, FixPoint1),
			prove(Goal, FixPoint1, Satisfied, FixPoint)
		).

	satisfy_negative_literals([], _, []).
	satisfy_negative_literals([not(X)|Pending], FixPoint, Satisfied) :-
		(   \+ list::member(X, FixPoint) ->
			Satisfied = [not(X)|Satisfied1],
			satisfy_negative_literals(Pending, FixPoint, Satisfied1)
		;	satisfy_negative_literals(Pending, FixPoint, Satisfied)						
		).

	subsumption_iterate(Goal, _, DI, _, _, _) :-
		list::member(Goal, DI).
	subsumption_iterate(Goal, I, DI, Pending0, Pending, Fix) :-
		debug((
			write('I is: '), writeln(I),
			write('DI is: '), writeln(DI),
			write('Pending0 is: '), writeln(Pending0)
		)),
		subsumption_next(I, DI, NextI, NextDi, NextPending),
		(	NextDi = [], NextPending = [] ->
			Fix = NextI,
			Pending = Pending0
		;	list::append(NextPending, Pending0, Pending1),
			list::sort(Pending1, Pending2),
			subsumption_iterate(Goal, NextI, NextDi, Pending2, Pending, Fix)
		).

	subsumption_next(I, Di, NextI, NextDi, Pending) :-
		collect(I, Di, Tmp, Pending),
		subsumption_sort(Tmp, NextDi),
		subsumption_union(I, NextDi, NextI).

	collect(I, Di, Heads, Pendings) :-
		findall(
			Head,
			(database::rule(Head, Body, PosOrNeg),
			 satisfy_one(Body, Di, NewBody),
			 debug((write(rule(Head, Body, PosOrNeg)),nl)),
			 satisfy_all(NewBody, I, []),
			 \+ subsumption_member(Head, I)),
			Heads),
		findall(
			Pending,
			(database::rule(Head, Body, negative),
			 satisfy_one(Body, Di, NewBody),
			 satisfy_all(NewBody, I, [Pending])),
			Pendings).

	subsumption_member(X, [Y|Ys]) :-
		(	subsumed(X, Y) ->
			true
		;	X @>= Y,
			subsumption_member(X, Ys)
		).

	subsumption_sort([], []) :-
		!.
	subsumption_sort([X], [X]) :-
		!.
	subsumption_sort(UnSorted, Sorted) :-
		split(UnSorted, UnSorted1, UnSorted2),
		subsumption_sort(UnSorted1, Sorted1),
		subsumption_sort(UnSorted2, Sorted2),
		subsumption_union(Sorted1, Sorted2, Sorted).

	subsumption_union([], Xs, Xs) :-
		!.
	subsumption_union(Xs, [], Xs) :-
		!.
	subsumption_union([X|Xs], [Y|Ys], Zs) :-
		subsumed(Y, X), !, subsumption_union([X|Xs], Ys, Zs).
	subsumption_union([X|Xs], [Y|Ys], Zs) :-
		subsumed(X, Y), !, subsumption_union(Xs, [Y|Ys], Zs).
	subsumption_union([X|Xs], [Y|Ys], [X|Zs]) :-
		X @< Y, !, subsumption_union(Xs, [Y|Ys], Zs).
	subsumption_union([X|Xs], [Y|Ys], [Y|Zs]) :-
		subsumption_union([X|Xs], Ys, Zs).

	satisfy_one([X|Xs], I, Xs) :-
		\+ database::builtin(X),
		satisfy_atom(I, X).
	satisfy_one([X|Xs], I, [X|Ys]) :-
		satisfy_one(Xs, I, Ys).

	satisfy_all([], _, []).
	satisfy_all([not(X)|Xs], Int, Pending) :-
		!,
		(   satisfy_atom(Int, not(X)) ->
			satisfy_all(Xs, Int, Pending)
		;	satisfy_atom(Int, X) -> 
			fail
		;	Pending = [not(X)]
		).
						  
	satisfy_all([X|Xs], Int, Pending) :-
		satisfy_atom(Int, X),
		satisfy_all(Xs, Int, Pending).

	satisfy_atom(_,A) :-
		database::builtin(A), 
		!,
		database::A.

	satisfy_atom([X| Xs], A) :-
		(	copy_term(X, A)
		;	satisfy_atom(Xs, A)
		).

	split([], [], []).
	split([X|Xs], [X|Ys], Zs) :-
		split(Xs, Zs, Ys).

    %%The double negation is a dirty hack to avoid binding any variables.
    subsumed(X, Y) :-
        \+ \+ term::subsumes(X, Y).    

:- end_object.
