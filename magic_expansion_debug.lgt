
:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(magic_expansion_debug,
	implements(expanding),
	extends(magic_expansion)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/04/15,
		comment is 'Expands rules of the form p <- f & g to the more manageable rule(p, [f,g]) and performs magic transformation of clauses.']).

	goal_expansion(debug(Goal), Goal).

	term_expansion(Term, Expansion) :-
		^^term_expansion(Term, Expansion).

:- end_object.
