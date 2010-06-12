
:- object(demodb,
	implements(databasep)).

	:- dynamic.

	:- discontiguous(rule/4).
	:- discontiguous(rule/3).
	:- discontiguous(bench_goal/1).
	:- discontiguous(builtin/1).

/* Remove these later.
	builtin(write(_)).
	builtin(nl).
	builtin(compare(_,_,_)).
	builtin(_ = _).
	builtin(_ \= _).
	builtin(fail).
	builtin(_ is _).
	builtin(_ =\= _).
	builtin(_ < _).
	builtin(_ =< _).
	builtin(_ > _).
	builtin(_ >= _).
	builtin(_ \== _).
*/

	%%Some simple test programs.

	append([], Ys, Ys) <- true.
	append([X|Xs], Ys, [X|Zs]) <-
		append(Xs, Ys, Zs).
  
	nrev([], []) <- true.
	nrev([X|Xs], Reversed) <-
		nrev(Xs, Reversed1) &
		append(Reversed1, [X], Reversed).
	
	length([], 0) <- true.
	length([_|Xs], N) <-
		length(Xs, N0) &
		 {N is N0 + 1}.

:- end_object.
