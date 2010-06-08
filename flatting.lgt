
:- op(1000, xfy, (&)).

:- category(flatting).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/04/15,
		comment is 'Flattens conjunction of goals with the form f & g into a list [f,g].']).

	:- protected(flatten_goals//1).

	flatten_goals((G1 & G2)) -->
		!,
		flatten_goals(G1),
		flatten_goals(G2).
	flatten_goals(true) -->
		!,
		[].
	flatten_goals(G) -->
		[G].

:- end_category.
