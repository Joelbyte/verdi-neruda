
:- object(database).

    :- public(rule/3).

    member(X, [X|_]) <- true.
    member(X, [_|Xs]) <-
        member(X, Xs).

    append([], Ys, Ys) <- true.
    append([X|Xs], Ys, [X|Zs]) <-
        append(Xs, Ys, Zs).        

    nrev([], []) <- true.
    nrev([X|Xs], Reversed) <-
        nrev(Xs, Reversed1) &
        append(Reversed1, [X], Reversed).

    f(X) <- g(X).
    f(2) <- true.

    g(1) <- true.

    edge(a, b) <- true.
    edge(b, c) <- true.

    connected(X, Y) <- edge(X, Y).
    connected(X, Z) <-
        edge(X,Y) &
        connected(Y, Z).

:- end_object.
