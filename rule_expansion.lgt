:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(rule_expansion,
	implements(expanding)).		

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Expands rules of the form p <- f & g to the more manageble
		rule(p, [f,g]).']).

	term_expansion((Head <- Goals), rule(Head, List)) :-
	    flatten_goals(Goals, List, []).

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
