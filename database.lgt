
:- object(database).

    :- dynamic.

    :- public(rule/4).
    :- public(rule/3).
    :- public(rule/2).
    :- public(builtin/1).

    builtin(plus(_, _, _)).
    builtin(write(_)).
    builtin(nl).
    builtin(compare(_,_,_)).
    builtin(_ = _).
    builtin(_ \= _).
    builtin(fail).
    builtin(append(_,_,_)).
    builtin(_ is _).
    builtin(_ =\= _).
    builtin(_ < _).
    builtin(_ =< _).
    builtin(_ > _).
    builtin(_ >= _).

    % append([], Ys, Ys) <- true.
    % append([X|Xs], Ys, [X|Zs]) <-
    %     append(Xs, Ys, Zs).        

    %%Benchmark 1 - naive reverse.
/*    nrev([], []) <- true.
    nrev([X|Xs], Reversed) <-
        nrev(Xs, Reversed1) &
        append(Reversed1, [X], Reversed).
*/

    %% Benchmark 2 - transitive closure of edge/2.
/*    edge(a, b) <- true.
    edge(a, c) <- true.
    edge(b, c) <- true.
    edge(c, d) <- true.


    connected(X, Z) <-     
        edge(X,Y) &
        connected(Y, Z).
    connected(X, Y) <- edge(X, Y).
*/

    %% Benchmark 3 - calculating the n:th Fibonacci number recursively.
/*
    fib_rec(1, 1) <- true.
    fib_rec(2, 1) <- true.
    fib_rec(N, X) <- 
        compare(>, N, 2) & 
        plus(N, -1, N1) &
        fib_rec(N1, X1) &
        plus(N, -2, N2) &
        fib_rec(N2, X2) &
        plus(X1, X2, X).
*/
    %% Benchmark 3 - calculating the n:th Fibonacci number iteratively.
/*
    fib_iter(1, 1) <- true.
    fib_iter(2, 1) <- true.
    fib_iter(N, X) <- 
        compare(>, N, 2) &
        fib_iter(2, N, 1, 1, X).

    fib_iter(N, N, X, _, X) <- true.
    fib_iter(N0, N, X2, X1, X) <-
        compare(<, N0, N) &
        plus(N0, 1, N1) &
        plus(X2, X1, X3) &
        fib_iter(N1, N, X3, X2, X).
*/
    %% Benchmark 4 - calculating the n:th Fibonacci number with a generator.
    %% Perhaps it is not necessary?
/*
    fib_gen(N, X) <- 
        make_fibonacci(State0) &
        fib_gen(N, State0, X).

    fib_gen(0, s0, 1) <- true.
    fib_gen(0, s1(1), 1) <- true.
    fib_gen(0, s2(X, _), X) <- true.
    fib_gen(N, State0, X) <- 
        compare(>, N, 0) &
        fibonacci_next(_, State0, State) &
        plus(N, -1, N0) &
        fib_gen(N0, State, X).

    make_fibonacci(s0) <- true.

    fibonacci_next(Fib, State0, State) <-
        next_fibonacci(State0, Fib, State).

    next_fibonacci(s0, 1, s1(1)) <- true.
    next_fibonacci(s1(X1), 1, s2(1, X1)) <- true.
    next_fibonacci(s2(X2, X1), X3, s2(X3, X2)) <-
        plus(X2, X1, X3).
*/

    %% Benchmark 5 - Determining whether two binary trees are isomorphic.
    %% TODO: Decide how the trees should be generated.
/*
    isotree(void, void) <- true.
    isotree(t(X, L1, R1), t(X, L2, R2)) <-
        isotree(L1, L2) &
        isotree(R1, R2).
    isotree(t(X, L1, R1), t(X, L2, R2)) <-
        isotree(L1, R2) &
        isotree(R1, L2).
*/
    %% Benchmark 6 - Parsing natural language with a DCG.

    %% Benchmark 7 - solving the mu-puzzle from GEB.
/*
    theorem(_, [m, i]) <- true.
    theorem(_, []) <- fail.
    theorem(Depth, R) <- 
        compare(>, Depth, 0) &
        plus(Depth, -1, D) &
        theorem(D, S) &
        rules(S, R).

    rules(S, R) <- rule1(S, R).
    rules(S, R) <- rule2(S, R).
    rules(S, R) <- rule3(S, R).
    rules(S, R) <- rule4(S, R).

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
*/
    %%Benchmark 8 - Solving the 4-queen puzzle.
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

    select([X|Xs],Xs,X) <- true.
    select([Y|Ys],[Y|Zs],X) <- select(Ys,Zs,X).

    range(N,N,[N]) <- true.
    range(M,N,[M|Ns]) <-
    	M < N &
    	M1 is M+1 &
    	range(M1,N,Ns).
*/
    %%Benchmark 9 - Database test for finding related regions.
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
    area(china,     3380)<- true.
    area(india,     1139)<- true.
    area(ussr,      8708)<- true.
    area(usa,       3609)<- true.
    area(indonesia,  570)<- true.
    area(japan,      148)<- true.
    area(brazil,    3288)<- true.
    area(bangladesh,  55)<- true.
    area(pakistan,   311)<- true.
    area(w_germany,   96)<- true.
    area(nigeria,    373)<- true.
    area(mexico,     764)<- true.
    area(uk,          86)<- true.
    area(italy,      116)<- true.
    area(france,     213)<- true.
    area(philippines, 90)<- true.
    area(thailand,   200)<- true.
    area(turkey,     296)<- true.
    area(egypt,      386)<- true.
    area(spain,      190)<- true.
    area(poland,     121)<- true.
    area(s_korea,     37)<- true.
    area(iran,       628)<- true.
    area(ethiopia,   350)<- true.
    area(argentina, 1080)<- true.
*/

    %% Benchmark 10 - negation test. Compare if there are any differences between member/2 and nonmember/2.
/*    
    eq(X, X) <- true.

    member(X, [X|_]) <- true.
    member(X, [_|Xs]) <-
         member(X, Xs).

    nonmember(_, []) <- true.
    nonmember(X, [Y|Ys]) <-
        not(eq(X, Y)) &
        nonmember(X, Ys).
*/

    %     a <- true.
    %     b <- a.
    %     c <- b.
    %     d <- c.
    %     e <- d.
    %     f <- e & e.
    %     f <- a & a & a.

    %     heuristic_test <-
    %         f.

    % %%stratification test
    % %e(X, Y) <- c(X) & a(X) & h(X, Y) & a(Y).

    % a(X) <- g(X).
    % a(X) <- b(X, Y) & a(Y).

    % c(a) <- true.

    % g(a) <- true.

    % b(a, a) <- true.

    % h(a, a) <- true.
    % h(b, a) <- true.

    % d(b) <- true.


    % %f(X) <- not(a(X)) & h(X, Y) & a(Y).

:- end_object.
