
:- object(shell(_Interpreters)).

    :- dynamic.

    :- info([
        version is 0.1,
        author is 'Victor Lagerkvist',
        date is 2010/03/23,
        comment is 'Prolog shell for the interpreters.']).

    :- public(init/0).
    :- public(dispatch/1).

    init :-
        writeln('Welcome, noble adventurer, your destiny awaits you.'),
        writeln('Type help. for online help'),
        repl.
    
    repl :-
        write('>> '),
        read(X),
        dispatch(X).
    repl :-
        repl. 

    dispatch(halt).
    dispatch(help) :-
        write_help_message,
        repl.
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
        member(Interpreter, Interpreters),
        Interpreter::prove(Goal),
        writeln(Goal).

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
        
:- end_object.
