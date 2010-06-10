%%Simple global counter using asserta/retract.
%%TODO: Add info directives.

:- object(counter).

   :- dynamic.

   :- initialization(init).

   :- public(increment/0).
   :- public(increase/1).
   :- public(set/1).
   :- public(value/1).
   :- public(reset/0).

   :- private(c/1).
   :- dynamic(c/1).

	init :-
		retractall(c(_)),
		asserta(c(0)).

	increment :-
		retract(c(N0)),
		N is N0 + 1,
		asserta(c(N)).
   
	increase(I) :-
		retract(c(N0)),
		N is N0 + I,
		asserta(c(N)).   

	set(N) :-
		retract(c(_)),
		asserta(c(N)).   

	value(N) :- 
		c(N).

	reset :- 
		retract(c(_)),
		asserta(c(0)).

:- end_object.
