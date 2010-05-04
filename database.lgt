
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
/*
    nrev([], []) <- true.
    nrev([X|Xs], Reversed) <-
        nrev(Xs, Reversed1) &
        append(Reversed1, [X], Reversed).

    bench(nrev("abcde", "edcba")) <- true.
    bench(nrev("abcdefghij", "jihgfedcba")) <- true.
    permute([nrev(_, _) <- all]).
*/

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

    connected(X, Z) <-     
        edge(X,Y) &
        connected(Y, Z).
    connected(X, Y) <- edge(X, Y).

    bench(connected(1, 2)) <- true.
    bench(connected(1, 3)) <- true.
    bench(connected(1, 4)) <- true.
    bench(connected(1, 5)) <- true.
    bench(connected(1, 6)) <- true.
    bench(connected(1, 7)) <- true.
    bench(connected(1, 8)) <- true.
    bench(connected(1, 9)) <- true.

    permute([connected(_, _) <- all]).
*/

    %% Benchmark 3 - calculating the n:th Fibonacci number recursively.

/*
    fib_rec(1, 1) <- true.
    fib_rec(2, 1) <- true.
    fib_rec(N, X) <- 
        N > 2 & 
        N1 is N - 1 &
        fib_rec(N1, X1) &
        N2 is N - 2 &
        fib_rec(N2, X2) &
        X is X1 + X2. 

    bench(fib_rec(5, 5)) <- true.
    bench(fib_rec(10, 55)) <- true.

    permute([fib_rec(_, _) <- [N > 2]]).
*/

    %% Benchmark 3 - calculating the n:th Fibonacci number iteratively.
/*
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

    bench(fib_iter(5, 5)) <- true.
    bench(fib_iter(10, 55)) <- true.

    permute([fib_iter(_, _, _, _, _) <- [_ < _]]).
*/
    %% Benchmark 4 - Determining whether two binary trees are isomorphic.
    %% TODO: Decide how the trees should be generated.

/*
    isotree(void, void) <- true.
    isotree(t(X, L1, R1), t(X, L2, R2)) <-
        isotree(L1, L2) &
        isotree(R1, R2).
    isotree(t(X, L1, R1), t(X, L2, R2)) <-
        isotree(L1, R2) &
        isotree(R1, L2).

    tree1(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(0, t(3, void, void), t(0, void, void)), t(2, t(4, void, void), t(3, t(2, void, void), t(3, void, void)))))) <- true.

    tree1_iso(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(2, t(3, t(2, void, void), t(3, void, void)), t(4, void, void)), t(0, t(0, void, void), t(3, void, void))))) <- true.

    tree2(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))) <- true.

    tree2_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(2, void, void)))))) <- true.

    tree3(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))) <- true.

    tree3_non_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(x, void, void)))))) <- true.
 
    bench(isotree(T, IsoT)) <- tree1(T) & tree1_iso(IsoT).
    bench(isotree(T, IsoT)) <- tree2(T) & tree2_iso(IsoT).
    bench(isotree(T, NonIsoT)) <- tree3(T) & tree3_non_iso(NonIsoT).

    permute([isotree(_, _) <- all]).
*/
    %% Benchmark 5 - Parsing natural language with a DCG.

    %% Benchmark 6 - solving the mu-puzzle from GEB.

/*
    theorem(_, [m, i]) <- true.
    theorem(_, []) <- fail.
    theorem(Depth, R) <- 
        Depth > 0 &
        D is Depth - 1 &
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

    test_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u]) <- true.
    test_non_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u, x, u, i, u, i, u, i, u]) <- true.

    bench(theorem(5, T)) <- test_theorem(T).
    bench(theorem(5, T)) <- test_non_theorem(T).

    permute([rules(_, _) <- all, rule1(_, _) <- all, rule2(_, _) <- all, rule3(_, _) <- all, rule4(_, _) <- all]).
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

    select([X|Xs],Xs,X) <- true.
    select([Y|Ys],[Y|Zs],X) <- select(Ys,Zs,X).

    range(N,N,[N]) <- true.
    range(M,N,[M|Ns]) <-
    	M < N &
    	M1 is M+1 &
    	range(M1,N,Ns).

    bench(queens(4, [2, 4, 1, 3])) <- true.
    bench(queens(4, [2, 4, 3, 1])) <- true.

    permute([not_attack(_, _, _) <- [not_attack(_, _, _)], select <- all, range <- all]).
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

    bench(query([ethiopia, 77, mexico, 76])) <- true.
    bench(query([france, 246, iran, 628])) <- true.

    permute([]).
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

    bench(member(0'e, "abcde")) <- true.
    bench(member(0'e, "abcdd")) <- true.
    bench(nonmember(0'e, "abcdd")) <- true.
    bench(nonmember(0'e, "abcde")) <- true.

    permute([member(_, _) <- all, nonmember(_, _) <- all]).
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
