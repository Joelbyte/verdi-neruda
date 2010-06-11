
:- object(shell(_Interpreters)).

	:- info([
		version is 0.1,
		author is 'Victor Lagerkvist and Paulo Moura',
		date is 2010/03/23,
		comment is 'Prolog shell for the interpreters.',
		parnames is ['Interpreters']]).

	:- public(init/0).
 
	init :-
		write('Welcome, noble adventurer, your destiny awaits you.'), nl,
		write('Type help. for online help'), nl,
		repl.
	
	repl :-
		load_database(rule_expansion(production)),
		write('>> '),
		read(X),
		user_reply(X),
		repl.
	repl :-
		write('no'), nl,
		repl.

	user_reply(X) :-
		functor(X,  F, _),
		(	F = prove ->						
			dispatch(X),
			write('Type \'y.\' or \'n.\' followed by return '), nl,
			(read(n) -> fail ; !)
		;	dispatch(X)
		).
			  
	command(halt, 'Shuts down the Prolog system.').
	command(help, 'Prints this message.').
	command(listing, 'Prints the currently loaded rules.').
	command(programs, 'Prints the currently loaded predicates.').
	command(interpreters, 'Prints a list of the available meta-interpreters.').
	command(prove('Interpreter', 'Goal'), 'Proves Goal with Interpreter.').
	command(prove('Interpreter', 'Goal', 'Limit'), 'Proves Goal with Interpreter if Limit is not exceeded.').
	command(benchmark_all('Statistic', 'N'), 'Benchmarks all interpreters with Statistic N times. Benchmarks are stored in the database as bench_goal/1 facts or rules.').
	command(benchmark('Interpreter', 'Statistic', 'N', 'Goal'), 'Benchmarks Interpreter with respect to Statistic, N and Goal.').

	dispatch(halt) :-
		halt.
	dispatch(help) :-
		write_help_message.
	dispatch(listing) :-
		findall(builtin(X), 
				(
				 database::builtin(X),
				 numbervars(X, 0, _)
				),
				Builtins),
		findall(rule(Head, Body), 
			    (
				 database::rule(Head, Body), 
				 numbervars(rule(Head, Body), 0, _)
				), 
			   Rules),
		meta::map(write_builtin, Builtins),
		meta::map(write_rule, Rules).
	dispatch(programs) :-
		findall(
			Functor/Arity,
			(database::rule(Head, _), functor(Head, Functor, Arity)),
			Functors),
		list::sort(Functors, SortedFunctors),
		meta::map(writeln, SortedFunctors).
	dispatch(interpreters) :-
		findall(
			Interpreter,
			(	current_object(Interpreter),
				Interpreter::current_predicate(prove/1),
				Interpreter::current_predicate(prove/2)
			),
			Interpreters),
		meta::map(writeln, Interpreters).
	dispatch(prove(Interpreter, Goal)) :-
		valid_interpreter(Interpreter, Expander),						
		load_database(Expander),
		prove(Interpreter, Goal).
	dispatch(prove(Interpreter, Goal, Limit)) :-
		valid_interpreter(Interpreter, Expander),		
		load_database(Expander),
		prove(Interpreter, Goal, Limit).
	
	:- if(predicate_property(statistics(_,_), built_in)).

		dispatch(benchmark_all(Statistic, N)) :-
			open('results.txt', append, Stream),
			(	valid_interpreter(Interpreter, Expander),
				nl(Stream),
				write(Stream, Interpreter),
				write(Stream, ':'),
				database::bench_goal(Goal), %Assumes a set of bench_goal/1 clauses in the database.
				load_database(Expander),
				write_benchmark(Stream, Interpreter, Statistic, N, Goal),
				fail
			;	write('Done.'), nl, 
				close(Stream)
			).

	:- endif.

	dispatch(benchmark_all) :-
		open('results.txt', append, Stream),
		(	valid_interpreter(Interpreter, Expander),
			nl(Stream),
			write(Stream, Interpreter),
			write(Stream, ':'),
			database::bench_goal(Goal), %Assumes a set of bench_goal/1 clauses in the database.
			load_database(Expander),
			write_benchmark(Stream, Interpreter, Goal),
			fail
		;	write('Done.'), nl, 
			close(Stream)
		).

	:- if(predicate_property(statistics(_,_), built_in)).

		dispatch(benchmark(Interpreter, Statistic, N, Goal)) :-
			valid_interpreter(Interpreter, Expander),
			load_database(Expander),
			(	benchmark(Interpreter, Statistic, N, Goal, Res0)
			;	benchmark_failure(Interpreter, Statistic, N, Goal, Res0),
				write('(failure) ')
			),
			write(Statistic), write(': '),
			Res is Res0/N,
			write(Res), nl.

	:- endif.	

	dispatch(benchmark(Interpreter, Goal)) :-
		valid_interpreter(Interpreter, Expander),
		load_database(Expander),
		current_output(Stream),
		write(Stream, Interpreter),
		write_benchmark(Stream, Interpreter, Goal),
		nl.
	dispatch((Goal, Goals)) :-
		dispatch(Goal),
		dispatch(Goals).
	dispatch(Goal) :-
		prove(dfs_interpreter, Goal).

	:- if(predicate_property(statistics(_,_), built_in)).
		
		benchmark(_, _, 0, _, 0) :- !.
		benchmark(Interpreter, Statistic, N, Goal, Res) :-
			N1 is N - 1,
			benchmark(Interpreter, Statistic, N1, Goal, Res0),
			statistics(Statistic, Before),
			Interpreter::prove(Goal, 1000000), !,
			statistics(Statistic, After),
			Res is Res0 + (After - Before).

		benchmark_failure(_, _, 0, _, 0) :- !.
		benchmark_failure(Interpreter, Statistic, N, Goal, Res) :-
			N1 is N - 1,
			benchmark_failure(Interpreter, Statistic, N1, Goal, Res0),
			statistics(Statistic, Before),
			\+ Interpreter::prove(Goal, 1000000), !,
			statistics(Statistic, After),
			Res is Res0 + (After - Before).
	:- endif.

	benchmark(Interpreter, Goal, Inferences) :-
		counter::reset,
		Interpreter::prove(Goal, 1000000), !,
		counter::value(Inferences).

	benchmark_failure(Interpreter, Goal, Inferences) :-
		counter::reset,
		\+ Interpreter::prove(Goal, 1000000), !,
		counter::value(Inferences).		

	prove(Interpreter, Goal) :-
		Interpreter::prove(Goal),
		write(Goal), nl.
	prove(Interpreter, Goal, Limit) :-
		Interpreter::prove(Goal, Limit),
		write(Goal), nl.

	valid_interpreter(Interpreter, Expander) :-
		this(shell(Interpreters)),
		functor(Interpreter, Functor, Arity),			% necessary for the parametric
		functor(InterpreterTemplate, Functor, Arity),	% interpreters
		list::member(InterpreterTemplate - Expander, Interpreters).

	load_database(Expander) :-
		logtalk_load(database, [hook(Expander), report(off), plredef(silent), unknown(silent), lgtredef(silent), startup_message(none)]). 
	
	write_statistics(Stream, Statistic, N, Res0) :-
		debug((write(Stream, Statistic), write(Stream, ': '))),
		Res1 is Res0/N,
		Res is floor(Res1),
		write(Stream, Res).

	write_help_message :-
		write('Available commands are:'), nl,
		(	command(Command, Description),
			write(Command), write(' : '), write(Description), nl,
			fail
		;	true
		).

	%%TODO: Make the output prettier.
	write_builtin(X) :- 
		write(X), nl.	
		
	write_rule(rule(Head, Body)) :-
		write(Head),
		write(' '),
		write('<-'), nl,
		write_body(Body),
		nl.

	write_body([G]) :-
		!,
		write('	  '),
		write(G),
		write('.').
	write_body([]) :-
		!,
		write('	  '),
		write(true),
		write('.').
	write_body([G|Gs]) :-
		write('	  '),	
		write(G),
		write(' '),
		write('&'), nl,
		write_body(Gs).

	write_benchmark(Stream, Interpreter, Statistic, N, Goal) :-
		write(Stream, ' '), 
		(	benchmark(Interpreter, Statistic, N, Goal, Res), !
		;	benchmark_failure(Interpreter, Statistic, N, Goal, Res),
			write(Stream, '(F) ')
		),
		write_statistics(Stream, Statistic, N, Res).
		
	write_benchmark(Stream, Interpreter, Goal) :-
		write(Stream, ' '), 
		(	benchmark(Interpreter, Goal, Inferences), !
		;	benchmark_failure(Interpreter,  Goal, Inferences),
			write(Stream, '(F) ')
		),
		write('inferences: '),
		write(Stream, Inferences).

	writeln(X) :-
		write(X),
		nl.		

	writeln(Stream, X) :-
		write(Stream, X),
		nl(Stream).

:- end_object.
