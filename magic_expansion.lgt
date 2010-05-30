:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(magic_expansion,
        implements(expanding)).

    :- info([
        version is 0.1,
        author is 'Victor Lagerkvist',
        date is 2010/04/15,
        comment is 'Expands rules of the form p <- f & g to the more manageable
        rule(p, [f,g]) and performs magic transformation of clauses.']).

    :- protected(flatten_goals/3).
    :- public(term_expansion/2).

    term_expansion(builtin(Goal), [builtin(Goal), NewPredicate]) :-
        NewPredicate =.. [(:-), Goal, user::Goal].

    term_expansion((Head <- Goals), MagicClauses) :-
        findall(rule(MagicHead, MagicBody, NegOrPos),
                (
                    flatten_goals(Goals, Body, []),
                    magic::magicise(Head, Body, MagicHead, MagicBody),
                    (   member(not(_), MagicBody) ->
                            NegOrPos = negative
                        ;
                            NegOrPos = positive
                    )
                ),
                MagicClauses), write('MagicClauses are: '), writeln(MagicClauses).

    flatten_goals((G1 & G2)) -->
        !,
        flatten_goals(G1),
        flatten_goals(G2).
    flatten_goals(true) -->
        !,
        [].
    flatten_goals(G) -->
        [G].
:- end_object.
