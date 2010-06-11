
:- protocol(interpreterp).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Protocol for an interpreter.']).

	:- public(prove/1).
	:- mode(prove(+goal), zero_or_more).
	:- info(prove/1, [
		comment is 'True if goal is provable.',
		argnames is ['Goal']]).

	:- public(prove/2).
	:- mode(prove(+goal, +limit), zero_or_more).
	:- info(prove/2, [
		comment is 'True if goal is provable within the given depth-limit.',
		argnames is ['Goal', 'Limit']]).

:- end_protocol.
