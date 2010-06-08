
:- object(magic).

	:- info([
		version is 0.1,
		author is 'Ulf Nilsson. Ported to Logtalk and augmented with stratified negation by Victor Lagerkvist.',
		date is 2010/04/15,
		comment is 'Object encapsulating magic methods.']).

	:- public(magicise/4).
	:- mode(magicise(+term, +list, -term, -list), zero_or_one).
	:- info(magicise/4, [
		comment is 'Transform (Head :- Body) into a magic clause (NewHead :- NewBody).',
		argnames is ['Head', 'Body', 'NewHead', 'NewBody']]).

	:- public(magic/2).
	:- mode(magic(+callable, -callable), zero_or_one).
	:- info(magic/2, [
		comment is 'Prefix the predicate symbol of Old with magic_.',
		argnames is ['Old', 'New']]).

	magicise(Head,Body,Head,[X|Body]) :-
		magic(Head,X).
	magicise(Head,Body,NewHead,[X|Left]) :-
		magic(Head,X),
		list::append(Left,[Y|_],Body),
		\+ database::builtin(Y),
		magic_head(Y,NewHead).
		
	magic_head(not(X), Y) :-
		!,
		magic_head(X, Y).
	magic_head(X, Y) :-
		magic(X, Y).

	magic(X, Y) :-
		nonvar(X),
		\+ database::builtin(X),
		X =.. [F | Args],
		atom_concat(magic_, F, G),
%		atom_codes(F, Z),				% PM: am I missing something here?
%		list::append("magic_", Z, W),
%		atom_codes(G, W),
		Y =.. [G | Args].

:- end_object.
