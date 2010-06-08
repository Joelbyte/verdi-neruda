
:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(rule_expansion_debug,
	implements(expanding),
	extends(rule_expansion)).

    :- info([
        version is 0.1,
        author is 'Victor Lagerkvist',
        date is 2010/03/18,
        comment is 'Expands rules of the form p <- f & g to the more manageable rule(p, [f,g]).']).

	goal_expansion(debug(Goal), Goal).

	term_expansion(Term, Expansion) :-
		^^term_expansion(Term, Expansion).

:- end_object.
