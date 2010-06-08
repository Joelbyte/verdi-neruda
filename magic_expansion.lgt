
:- op(1200, xfx, (<-)).
:- op(1000, xfy, (&)).

:- object(magic_expansion,
	implements(expanding),
	imports(flatting)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/04/15,
		comment is 'Expands rules of the form p <- f & g to the more manageable rule(p, [f,g]) and performs magic transformation of clauses.']).

	goal_expansion(debug(_), true).

	term_expansion(builtin(Goal), [builtin(Goal), (Goal :- {Goal})]).

	term_expansion((Head <- Goals), MagicClauses) :-
		findall(
			rule(MagicHead, MagicBody, NegOrPos),
			magic_clause(MagicHead, MagicBody, NegOrPos),
			MagicClauses).
%	debug((write('MagicClauses are: '), write(MagicClauses), nl)).

	magic_clause(MagicHead, MagicBody, NegOrPos) :-
		phrase(::flatten_goals(Goals), Body, []),
		magic::magicise(Head, Body, MagicHead, MagicBody),
		(   list::member(not(_), MagicBody) ->
			NegOrPos = negative
		;	NegOrPos = positive
		).

:- end_object.
