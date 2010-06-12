
:- protocol(databasep).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/18,
		comment is 'Database protocol.']).

	:- public(rule/4).
	:- mode(rule(?callable, ?callable, -, -), zero_or_more).
	:- info(rule/4,
		[comment is '.',
		 argnames is ['Head', 'Body', 'Clone', 'Tail']]).

	:- public(rule/3).
	:- mode(rule(?callable, ?callable, -), zero_or_more).
	:- info(rule/3,
		[comment is '.',
		 argnames is ['Head', 'Body', 'Tail']]).

	:- public(rule/2).
	:- mode(rule(?callable, -list(callable)), zero_or_more).
	:- info(rule/2, [
		comment is 'Clauses for this predicate are automatically generated using term-expansion.',
		argnames is ['Head', 'Body']]).

	:- public(builtin/1).
	:- mode(builtin(?callable), zero_or_more).
	:- info(builtin/1, [
		comment is 'Table of built-in predicates.',
		argnames is ['BuiltIn']]).

	:- public(bench_goal/1).
	:- mode(bench_goal(?callable), zero_or_more).
	:- info(bench_goal/1, [
		comment is 'Table of benchmark goals.',
		argnames is ['Goal']]).

:- end_protocol.
