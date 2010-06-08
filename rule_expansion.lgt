
:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(rule_expansion(Mode),
	implements(expanding),
	imports(flatting),
	extends(debug_expansion(Mode))).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Expands rules of the form p <- f & g to the more manageable rule(p, [f,g]).']).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion(builtin(Goal), (rule(Goal, {Goal}, []))).

	term_expansion((Head <- Goals), rule(Head, List, Tail)) :-
		phrase(::flatten_goals(Goals), List, Tail).

:- end_object.
