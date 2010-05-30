
:- object(shell(_Interpreters)).

    :- dynamic.

    :- info([
        version is 0.1,
        author is 'Victor Lagerkvist',
        date is 2010/03/23,
        comment is 'Prolog shell for the interpreters.']).

    :- public(init/0).
 
    init :-
        writeln('Welcome, noble adventurer, your destiny awaits you.'),
        writeln('Type help. for online help'),
        repl.
    
    repl :-
        load_database(rule_expansion),
        write('>> '),
        read(X),
        dispatch(X),
        (get_single_char(110) -> fail ; !),
        repl.
    repl :-
        writeln('no'),
        repl.

    dispatch(halt) :-
        halt.
    dispatch(help) :-
        write_help_message.
    dispatch(listing) :-
        findall(rule(Head, Body), database::rule(Head, Body, _), Rules),
        meta::map(write_rule, Rules).
    dispatch(programs) :-
        findall(Functor/Arity,
                (database::rule(Head, _, _),
                 functor(Head, Functor, Arity)),
                Functors),
        sort(Functors, SortedFunctors),
        meta::map(writeln, SortedFunctors).
    dispatch(prove(Interpreter, Goal)) :-
        this(shell(Interpreters)),
        member(Interpreter - Expander, Interpreters),
        load_database(Expander), 
        prove(Interpreter, Goal).
    dispatch(benchmark_all(Statistic, N)) :-
        user::open('results.txt', append, Stream),
        this(shell(Interpreters)),
        (   member(Interpreter - Expander, Interpreters),
%            writeln(Stream, '##########'),
            nl(Stream),
            %write(Stream, 'Interpreter: '), 
            write(Stream, Interpreter),
            database::bench_goal(Goal),
            %writeln(Stream, '#####'),
            %write(Stream, 'Goal: '),
            %writeln(Stream, Goal),
            load_database(Expander),
            write_benchmark(Stream, Interpreter, Statistic, N, Goal),
            writeln('Here'),
            fail
        ;   writeln('Done.'), 
            close(Stream)
        ).

    dispatch(benchmark(Interpreter, Statistic, N, Goal)) :-
        this(shell(Interpreters)),
        member(Interpreter - Expander, Interpreters),
        load_database(Expander),
        (   benchmark(Interpreter, Statistic, N, Goal, Res0)
        ;   benchmark_failure(Interpreter, Statistic, N, Goal, Res0),
            write('(failure) ')
        ),
        write(Statistic), write(': '),
        Res is Res0/N,
        writeln(Res).
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
        user::call_with_depth_limit(Interpreter::prove(Goal), 1000000, _), !,
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
        writeln(Goal).

    load_database(Expander) :-
        logtalk_load(database, [hook(Expander), report(off), plredef(silent), unknown(silent), lgtredef(silent), startup_message(none)]). 
    
    write_statistics(Stream, _Statistic, N, Res0) :-
        %write(Stream, Statistic), write(Stream, ': '),
        Res1 is Res0/N,
        Res is floor(Res1),
        write(Stream, Res).

    write_help_message :-
        writeln('Available commands are:'),
        findall(Command, ::clause(dispatch(Command), _), Commands),
        meta::map(writeln, Commands).
    
    write_rule(rule(Head, Body)) :-
        write(Head),
        write(' '),
        writeln('<-'),
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
        writeln('&'),
        write_body(Gs).

    write_benchmark(Stream, Interpreter, Statistic, N, Goal) :-
        write(Stream, ' & '), 
        (   benchmark(Interpreter, Statistic, N, Goal, Res), !
        ;   benchmark_failure(Interpreter, Statistic, N, Goal, Res),
            write(Stream, '(F) ')
        ),
        write_statistics(Stream, Statistic, N, Res).

    writeln(Stream, X) :-
        user::write(Stream, X),
        user::nl(Stream).        
:- end_object.
