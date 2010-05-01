
:- object(database).

    :- dynamic.

    :- public(rule/4).
    :- public(rule/3).
    :- public(rule/2).
    :- public(builtin/1).

    builtin(plus(_, _, _)).
    builtin(write(_)).

    % member(X, [X|_]) <- true.
    % member(X, [_|Xs]) <-
    %     member(X, Xs).

    % append([], Ys, Ys) <- true.
    % append([X|Xs], Ys, [X|Zs]) <-
    %     append(Xs, Ys, Zs).        

    % nrev([], []) <- true.
    % nrev([X|Xs], Reversed) <-
    %     nrev(Xs, Reversed1) &
    %     append(Reversed1, [X], Reversed).

%    f(X) <- g(X).
%    f(2) <- true.
%
%    g(1) <- true.

    % edge(a, b) <- true.
    % edge(a, c) <- true.
    % edge(b, c) <- true.
    % edge(c, d) <- true.


    % connected(X, Z) <-     
    %     edge(X,Y) &
    %     connected(Y, Z).
    % connected(X, Y) <- edge(X, Y).

    % eq(X, X) <- true.

    % rule(smaller(X, Y), T, T) :- X @< Y.
    % rule(smaller(X, Y), T, 0, T) :- X @< Y.

    % rule(plus(X, Y, Z), T, T) :- plus(X, Y, Z).
    % rule(plus(X, Y, Z), T, 0, T) :- plus(X, Y, Z).

    % rule(ord_union(X, Y, Z), T, T) :- ord_union(X, Y, Z).
    % rule(ord_union(X, Y, Z), T, 0, T) :- ord_union(X, Y, Z).

    % subsumption_sort([], []) <- true.
    % subsumption_sort([X], [X]) <- true.
    % subsumption_sort([X, Y|UnSorted], Sorted)  <-
    % 	split([X, Y|UnSorted], UnSorted1, UnSorted2) &
    % 	subsumption_sort(UnSorted1, Sorted1) &
    % 	subsumption_sort(UnSorted2, Sorted2) &
    % 	union(Sorted1, Sorted2, Sorted).

    % split([], [], []) <- true.
    % split([X|Xs], [X|Ys], Zs) <-
	%     split(Xs, Zs, Ys).

    % union([], Xs, Xs) <- true.
    % union(Xs, [], Xs) <- true.
    % union([X|Xs], [Y|Ys], [X|Zs]) <-
    % 	smaller(X, Y) &
    %     union(Xs, [Y|Ys], Zs).
    % union([X|Xs], [Y|Ys], [Y|Zs]) <-
    %     not(smaller(X, Y)) &
    % 	union([X|Xs], Ys, Zs).

    % man(a) <- true.
    % man(b) <- true.
    % married(a) <- true.
    % bachelor(X) <- man(X) & not(married(X)).

    % nonmember(_, []) <- true.
    % nonmember(X, [Y|Ys]) <-
    %     not(eq(X, Y)) &
    %     nonmember(X, Ys).
    %nonmember(X, Ys) <- not(member(X, Ys)).

    %    negation_test(Y) <-
    %        edge(X, Y) &
    %        not(edge(a, Y)) &
    %        not(connected(Y, c)).

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

    % bfs_test(X, Y) <- test1(X) & test2(X, Y).
    % test1(1) <- true.
    % test1(2) <- true.
    % test2(1, a) <- true.
    % test2(2, a) <- true.

    builtin_test(X, Y, Z) <-
        plus(X, Y, Z) &
        write(Z).

:- end_object.
