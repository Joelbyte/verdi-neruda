
:- object(shell(_Interpreters)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist',
		date is 2010/03/23,
		comment is 'Prolog shell for the interpreters.',
		parnames is ['Interpreters']]).

	:- public(init/0).
 
	init :-
		write('Welcome, noble adventurer, your destiny awaits you.'), nl,
		write('Type help. for online help'), nl,
		repl.
	
	repl :-
		load_database(rule_expansion),
		write('>> '),
		read(X),
		dispatch(X),
		(get_single_char(110) -> fail ; !),
		repl.
	repl :-
		write('no'), nl,
		repl.

	command(halt).
	command(help).
	command(listing).
	command(programs).
	command(prove('Interpreter', 'Goal')).
	command(benchmark_all('Statistic', 'N')).
	command(benchmark('Interpreter', 'Statistic', 'N', 'Goal')).

	dispatch(halt) :-
		halt.
	dispatch(help) :-
		write_help_message.
	dispatch(listing) :-
		findall(rule(Head, Body), database::rule(Head, Body, _), Rules),
		meta::map(write_rule, Rules).
	dispatch(programs) :-
		findall(
			Functor/Arity,
			(database::rule(Head, _, _), functor(Head, Functor, Arity)),
			Functors),
		list::sort(Functors, SortedFunctors),
		meta::map(writeln, SortedFunctors).
	dispatch(prove(Interpreter, Goal)) :-
		this(shell(Interpreters)),
		list::member(Interpreter - Expander, Interpreters),
		load_database(Expander), 
		prove(Interpreter, Goal).
	dispatch(benchmark_all(Statistic, N)) :-
		open('results.txt', append, Stream),
		this(shell(Interpreters)),
		(   list::member(Interpreter - Expander, Interpreters),
			debug((write(Stream, '##########'), nl)),
			nl(Stream),
			debug(write(Stream, 'Interpreter: ')), 
			write(Stream, Interpreter),
			database::bench_goal(Goal),
			debug((
				write(Stream, '#####'), nl,
				write(Stream, 'Goal: '), nl,
				write(Stream, Goal), nl)),
			load_database(Expander),
			write_benchmark(Stream, Interpreter, Statistic, N, Goal),
			write('Here'), nl,
			fail
		;   write('Done.'), nl, 
			close(Stream)
		).

	dispatch(benchmark(Interpreter, Statistic, N, Goal)) :-
		this(shell(Interpreters)),
		list::member(Interpreter - Expander, Interpreters),
		load_database(Expander),
		(   benchmark(Interpreter, Statistic, N, Goal, Res0)
		;   benchmark_failure(Interpreter, Statistic, N, Goal, Res0),
			write('(failure) ')
		),
		write(Statistic), write(': '),
		Res is Res0/N,
		write(Res), nl.
	dispatch((Goal, Goals)) :-
		dispatch(Goal),
		dispatch(Goals).
	dispatch(Goal) :-
		prove(dfs_interpreter, Goal).
	
	benchmark(_, _, 0, _, 0) :- !.
	benchmark(Interpreter, Statistic, N, Goal, Res) :-
		N1 is N - 1,
		benchmark(Interpreter, Statistic, N1, Goal, Res0),
		statistics(Statistic, Before),
		call_with_depth_limit(Interpreter::prove(Goal), 1000000, _), !,
		statistics(Statistic, After),
		Res is Res0 + (After - Before).

	benchmark_failure(_, _, 0, _, 0) :- !.
	benchmark_failure(Interpreter, Statistic, N, Goal, Res) :-
		N1 is N - 1,
		benchmark_failure(Interpreter, Statistic, N1, Goal, Res0),
		statistics(Statistic, Before),
		call_with_depth_limit(\+ Interpreter::prove(Goal), 1000000, _), !,
		statistics(Statistic, After),
		Res is Res0 + (After - Before).

	prove(Interpreter, Goal) :-
		Interpreter::prove(Goal),
		write(Goal), nl.

	load_database(Expander) :-
		logtalk_load(database, [hook(Expander), report(off), plredef(silent), unknown(silent), lgtredef(silent), startup_message(none)]). 
	
	write_statistics(Stream, Statistic, N, Res0) :-
		debug((write(Stream, Statistic), write(Stream, ': '))),
		Res1 is Res0/N,
		Res is floor(Res1),
		write(Stream, Res).

	write_help_message :-
		write('Available commands are:'), nl,
		(	command(Command),
			write(Command), nl,
			fail
		;	true
		).

	write_rule(rule(Head, Body)) :-
		write(Head),
		write(' '),
		write('<-'), nl,
		write_body(Body),
		nl.
	
	write_body([G]) :-
		!,
		write('   '),
		write(G),
		write('.').
	write_body([]) :-
		!,
		write('   '),
		write(true),
		write('.').
	write_body([G|Gs]) :-
		write('   '),   
		write(G),
		write(' '),
		write('&'), nl,
		write_body(Gs).

	write_benchmark(Stream, Interpreter, Statistic, N, Goal) :-
		write(Stream, ' & '), 
		(   benchmark(Interpreter, Statistic, N, Goal, Res), !
		;   benchmark_failure(Interpreter, Statistic, N, Goal, Res),
			write(Stream, '(F) ')
		),
		write_statistics(Stream, Statistic, N, Res).

	writeln(X) :-
		write(X),
		nl.		

	writeln(Stream, X) :-
		write(Stream, X),
		nl(Stream).

:- end_object.
