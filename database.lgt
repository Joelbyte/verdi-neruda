
:- object(database).

	:- dynamic.
	:- discontiguous(rule/4).
	:- discontiguous(rule/3).
	:- discontiguous(rule/2).
	:- discontiguous(bench_goal/1).
	:- discontiguous(builtin/1).
	:- public(rule/4).
	:- public(rule/3).
	:- public(rule/2).
	:- public(builtin/1).
	:- public(bench_goal/1).

	builtin(write(_)).
	builtin(nl).
	builtin(compare(_,_,_)).
	builtin(_ = _).
	builtin(_ \= _).
	builtin(fail).
	builtin(_ is _).
	builtin(_ =\= _).
	builtin(_ < _).
	builtin(_ =< _).
	builtin(_ > _).
	builtin(_ >= _).
	builtin(_ \== _).

	%%Used for testing inferences, remove later.
	f <- not(c).
	f <- b.

	b <- a.
	a <- true.

	%%Benchmark 1 - naive reverse.
	append([], Ys, Ys) <- true.
	append([X|Xs], Ys, [X|Zs]) <-
		append(Xs, Ys, Zs).
  
	nrev([], []) <- true.
	nrev([X|Xs], Reversed) <-
		nrev(Xs, Reversed1) &
		append(Reversed1, [X], Reversed).
	
	length([], 0) <- true.
	length([_|Xs], N) <-
		length(Xs, N0) &
		 N is N0 + 1.

	  % nrev([], []) <- true.
	  % nrev([X|Xs], Reversed) <-
	  %	 append(Reversed1, [X], Reversed) &
	  %	 nrev(Xs, Reversed1).

	bench_goal(nrev("abcde", "edcba")).
	bench_goal(nrev("abcdefghij", "jihgfedcba")).
  
	%% Benchmark 2 - transitive closure of edge/2.
/*
	edge(1, 3) <- true.
	edge(3, 5) <- true.
	edge(5, 7) <- true.
	edge(7, 9) <- true.

	edge(0, 2) <- true.
	edge(2, 4) <- true.
	edge(4, 6) <- true.
	edge(6, 8) <- true.

	edge(1, 0) <- true.
	edge(3, 2) <- true.
	edge(5, 4) <- true.
	edge(7, 8) <- true.
	edge(9, 8) <- true.

	edge(0, 3) <- true.
	edge(2, 5) <- true.
	edge(4, 7) <- true.
	edge(6, 9) <- true.

%	connected(X, Z) <-	 
%		edge(X,Y) &
%		connected(Y, Z).
%	connected(X, Y) <- edge(X, Y).

%	 connected(X, Z) <-	 
%		 connected(Y, Z) &
%		 edge(X,Y).
%	 connected(X, Y) <- edge(X, Y).

%	 connected(X, Y) <- edge(X, Y).
%	 connected(X, Z) <-	 
%		 edge(X,Y) &
%		 connected(Y, Z).

%	 connected(X, Y) <- edge(X, Y).
%	 connected(X, Z) <-	 
%		 connected(Y, Z) &
%		 edge(X,Y).

	bench_goal(connected(1, 2)).
	bench_goal(connected(1, 3)).
	bench_goal(connected(1, 4)).
	bench_goal(connected(1, 5)).
	bench_goal(connected(1, 6)).
	bench_goal(connected(1, 7)).
	bench_goal(connected(1, 8)).
	bench_goal(connected(1, 9)).
*/
	%% Benchmark 3 - calculating the n:th Fibonacci number recursively.

/*
	fib_rec(1, 1) <- true.
	fib_rec(2, 1) <- true.
	fib_rec(N, X) <- 
		N > 2 & 
		N1 is N - 1 &
		N2 is N - 2 &		
		fib_rec(N1, X1) &
		fib_rec(N2, X2) &
		X is X1 + X2. 

	bench_goal(fib_rec(5, 5)).
	bench_goal(fib_rec(20, 6765)).

	%% Benchmark 3 - calculating the n:th Fibonacci number iteratively.

	fib_iter(1, 1) <- true.
	fib_iter(2, 1) <- true.
	fib_iter(N, X) <- 
		N > 2 &
		fib_iter(2, N, 1, 1, X).

	fib_iter(N, N, X, _, X) <- true.
	fib_iter(N0, N, X2, X1, X) <-
		N0 < N &
		N1 is N0 + 1 &
		X3 is X2 + X1 &
		fib_iter(N1, N, X3, X2, X).

	bench_goal(fib_iter(5, 5)).
	bench_goal(fib_iter(20, 6765)).
*/
	%% Benchmark 4 - Determining whether two binary trees are isomorphic.

/*
	isotree(void, void) <- true.

	%Normal version.  
%	isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		isotree(L1, L2) &
%		isotree(R1, R2).
%	isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		isotree(L1, R2) &
%		isotree(R1, L2).

	%%Goals in body swapped.
%	 isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		 isotree(R1, R2) &
%		 isotree(L1, L2).
%	 isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		 isotree(R1, L2) &
%		 isotree(L1, R2).

	% %%Clauses swapped and goals in body swapped.
%	 isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		 isotree(R1, R2) &
%		 isotree(L1, L2).
%	 isotree(t(X, L1, R1), t(X, L2, R2)) <-
%		 isotree(R1, L2) &
%		 isotree(L1, R2).

	tree1(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(0, t(3, void, void), t(0, void, void)), t(2, t(4, void, void), t(3, t(2, void, void), t(3, void, void)))))).

	tree1_iso(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(2, t(3, t(2, void, void), t(3, void, void)), t(4, void, void)), t(0, t(0, void, void), t(3, void, void))))).

	tree2(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))).

	tree2_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(2, void, void)))))).

	tree3(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))).

	tree3_non_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(x, void, void)))))).
 
	bench_goal(isotree(T, IsoT)) :- tree1(T), tree1_iso(IsoT).
	bench_goal(isotree(T, IsoT)) :- tree2(T), tree2_iso(IsoT).
	bench_goal(isotree(T, NonIsoT)) :- tree3(T), tree3_non_iso(NonIsoT).
*/
	%% Benchmark 5 - Parsing natural language with a DCG.
/*
	sentence(A, C) <-
		noun_phrase(A, B) &
		verb_phrase(B, C).

	noun_phrase(A, B) <-
		noun_phrase2(A, B).
	noun_phrase(A, C) <-
		determiner(A, B) &
		noun_phrase2(B, C).

	verb_phrase(A, C) <-
		verb(A, B) &
		noun_phrase(B, C).
	verb_phrase(A, B) <-
		verb(A, B).

	verb([contains|A], A) <- true.
	verb([eats|A], A) <- true.

	noun([pieplate|A], A) <- true.
	noun([surprise|A], A) <- true.
	noun([man|A], A) <- true.

	adjective([decorated|A], A) <- true.
	adjective([corpulent|A], A) <- true.

	determiner([the|A], A) <- true.
	determiner([a|A], A) <- true.

	noun_phrase2(A, C) <-
		adjective(A, B) &
		noun_phrase2(B, C).
	noun_phrase2(A, B) <-
		noun(A, B).

	bench_goal(sentence([the, corpulent, man, contains, a, decorated, pieplate], [])).
	bench_goal(sentence([the, corpulent, man, contains, a, decorated, platepie], [])).
*/
	%% Benchmark 6 - solving the mu-puzzle from GEB.

/*
	append([], Ys, Ys) <- true.
	append([X|Xs], Ys, [X|Zs]) <-
		append(Xs, Ys, Zs).
  
	theorem(_, [m, i]) <- true.
	theorem(_, []) <- fail.
	theorem(Depth, R) <- 
		Depth > 0 &
		D is Depth - 1 &
		theorem(D, S) &
		rules(S, R).

%	rules(S, R) <- rule1(S, R).
%	rules(S, R) <- rule2(S, R).
%	rules(S, R) <- rule3(S, R).
%	rules(S, R) <- rule4(S, R).

	%%Order reversed.
%	rules(S, R) <- rule4(S, R).
%	rules(S, R) <- rule3(S, R).
%	rules(S, R) <- rule2(S, R).
%	rules(S, R) <- rule1(S, R).

	rule1(S, R) <-
		append(X, [i], S) &
		append(X, [i,u], R).
	
	rule2([m|T], [m|R]) <-
		append(T, T, R).

	rule3([], _) <-
		fail.

	rule3(R, T) <-
		append([i,i,i], S, R),
		append([u], S, T).
	rule3([H|T], [H|R]) <-
		rule3(T, R).

	rule4([], _) <-
		fail.

	rule4(R, T) <-
		append([u,u], T, R).

	rule4([H|T], [H|R]) <-
		rule4(T, R).

	test_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u]).
	test_non_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, x, u]).

	bench_goal(theorem(4, T)) :- test_theorem(T).
	bench_goal(theorem(4, T)) :- test_non_theorem(T).
*/
	%%Benchmark 7 - Solving the 4-queen puzzle.
/*
	queens(N,Qs) <-
		range(1,N,Ns) &
		queens(Ns,[],Qs).

	queens([],Qs,Qs) <- true.
	queens(UnplacedQs,SafeQs,Qs) <-
		select(UnplacedQs,UnplacedQs1,Q) &
		not_attack(SafeQs,Q) &
		queens(UnplacedQs1,[Q|SafeQs],Qs).

	not_attack(Xs,X) <-
		not_attack(Xs,X,1).

	not_attack([],_,_) <- true.
	not_attack([Y|Ys],X,N) <-
		X =\= Y+N & 
		X =\= Y-N &
		N1 is N+1 &
		not_attack(Ys,X,N1).
	
%	select([X|Xs],Xs,X) <- true.
%	select([Y|Ys],[Y|Zs],X) <- select(Ys,Zs,X).

	%%Clause order swapped.
	select([Y|Ys],[Y|Zs],X) <- select(Ys,Zs,X).
	select([X|Xs],Xs,X) <- true.

	range(N,N,[N]) <- true.
	range(M,N,[M|Ns]) <-
		M < N &
		M1 is M+1 &
		range(M1,N,Ns).

	bench_goal(queens(4, [2, 4, 1, 3])).
	bench_goal(queens(4, [2, 4, 3, 1])).
*/
	%%Benchmark 8 - Database test for finding related regions.
/*
	query([C1,D1,C2,D2]) <-
		density(C1,D1) &
		density(C2,D2) &
		D1 > D2 &
		T1 is 20*D1 &
		T2 is 21*D2 &
		T1 < T2.

	density(C,D) <-
		pop(C,P) &
		area(C,A) &
		D is (P*100)//A.

	% populations in 100000's
	pop(china,	8250)<- true.
	pop(india,	5863)<- true.
	pop(ussr,	2521)<- true.
	pop(usa,	2119)<- true.
	pop(indonesia,	1276)<- true.
	pop(japan,	1097)<- true.
	pop(brazil,	1042)<- true.
	pop(bangladesh,	 750)<- true.
	pop(pakistan,	 682)<- true.
	pop(w_germany,	 620)<- true.
	pop(nigeria,	 613)<- true.
	pop(mexico,	 581)<- true.
	pop(uk,		 559)<- true.
	pop(italy,	 554)<- true.
	pop(france,	 525)<- true.
	pop(philippines, 415)<- true.
	pop(thailand,	 410)<- true.
	pop(turkey,	 383)<- true.
	pop(egypt,	 364)<- true.
	pop(spain,	 352)<- true.
	pop(poland,	 337)<- true.
	pop(s_korea,	 335)<- true.
	pop(iran,	 320)<- true.
	pop(ethiopia,	 272)<- true.
	pop(argentina,	 251)<- true.

	% areas in 1000's of square miles
	area(china,	 3380)<- true.
	area(india,	 1139)<- true.
	area(ussr,	  8708)<- true.
	area(usa,	   3609)<- true.
	area(indonesia,	 570)<- true.
	area(japan,	  148)<- true.
	area(brazil,	3288)<- true.
	area(bangladesh,  55)<- true.
	area(pakistan,	 311)<- true.
	area(w_germany,	  96)<- true.
	area(nigeria,	373)<- true.
	area(mexico,	 764)<- true.
	area(uk,		  86)<- true.
	area(italy,	  116)<- true.
	area(france,	 213)<- true.
	area(philippines, 90)<- true.
	area(thailand,	 200)<- true.
	area(turkey,	 296)<- true.
	area(egypt,	  386)<- true.
	area(spain,	  190)<- true.
	area(poland,	 121)<- true.
	area(s_korea,	 37)<- true.
	area(iran,	   628)<- true.
	area(ethiopia,	 350)<- true.
	area(argentina, 1080)<- true.

	bench_goal(query([ethiopia, 77, mexico, 76])).
	bench_goal(query([france, 246, iran, 628])).
*/
	%% Benchmark 9 - negation test. Compare if there are any differences between member/2 and nonmember/2.
/*	
	eq(X, X) <- true.

	member(X, [X|_]) <- true.
	member(X, [_|Xs]) <-
		 member(X, Xs).

	nonmember(_, []) <- true.
	nonmember(X, [Y|Ys]) <-
		not(eq(X, Y)) &
		nonmember(X, Ys).

	%%Clause order swapped and the two goals in nonmember/2 swapped.
	% member(X, [_|Xs]) <-
	%	  member(X, Xs).
	% member(X, [X|_]) <- true.

	% nonmember(X, [Y|Ys]) <-
	%	 nonmember(X, Ys) &
	%	 not(eq(X, Y)).

	% nonmember(_, []) <- true.


	bench_goal(member(0'e, "abcde")).
	bench_goal(member(0'e, "abcdd")).
	bench_goal(nonmember(0'e, "abcdd")).
	bench_goal(nonmember(0'e, "abcde")).

	permute([member(_, _) <- all, nonmember(_, _) <- all]).
*/

/*
member(X, [X|_]) <- true.
member(X, [_|Xs]) <-
	member(X, Xs).

% nonmember(X, [Y|Ys]) <-
%	 nonmember(X, Ys) &
%	 not(eq(X, Y)).

transform(State1,State2,Plan) <- 
   transform(State1,State2, [State1], Plan).

transform(State,State,_,[]) <- true.
transform(State1,State2,Visited,[Action|Actions]) <-
   legal_action(Action,State1) &
   update(Action,State1,State) &
   not(member(State,Visited)) &
   transform(State,State2,[State|Visited],Actions).

%transform(State1,State2,Visited,[Action|Actions]) <-
%	choose_action(Action,State1, State2) &
%	update(Action,State1,State) &
%	not(member(State,Visited)) &
%	transform(State,State2,[State|Visited],Actions).
%
legal_action(to_place(Block,Y,Place),State) <- 
   on(Block,Y,State) &
   clear(Block,State) &
   place(Place) &
   clear(Place,State).
legal_action(to_block(Block1,Y,Block2),State) <- 
   on(Block1,Y,State) &
   clear(Block1,State) &
   block(Block2) &
   Block1 \== Block2 &
   clear(Block2,State).

%choose_action(Action, State1, State2) <-
%	suggest(Action, State2) & legal_action(Action, State1).
%choose_action(Action, State1, State2) <-
%	legal_action(Action, State1).

%suggest(to_place(X, Y, Z), State) <-
%	member(on(X, Z), State) & place(Z).
%suggest(to_block(X, Y, Z), State) <-
%	member(on(X, Z), State) & block(Z).
%
%clear(X,State) <- not(member(on(Y,X),State)).
clear(X,State) <- not(above(X, State)).
above(X, State) <- member(on(_, X), State).
on(X,Y,State) <- member(on(X,Y),State).

update(to_block(X,Y,Z),State,State1) <-
   substitute(on(X,Y), on(X,Z),State,State1).
update(to_place(X,Y,Z),State,State1) <-
   substitute(on(X,Y),on(X,Z),State,State1).

substitute(X,Y,[X|Xs],[Y|Xs]) <- true.
substitute(X,Y,[X1|Xs],[X1|Ys]) <-
	X \== X1 &
	substitute(X,Y,Xs,Ys).

block(a) <- true. block(b) <- true. block(c) <- true.

place(p) <- true. place(q) <- true. place(r) <- true.

initial_state(test, [on(a, b), on(b, p), on(c, r)]).
final_state(test, [on(a, b), on(b, c), on(c, r)]).
*/
%bench_goal(transform(I, F, Plan)) :-
%		initial_state(Name, I),
%		final_state(Name, F).

:- end_object.
