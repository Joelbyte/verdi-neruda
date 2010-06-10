load_interpreters([]).
load_interpreters([I|Is]) :-
	functor(I, Name, _),
	logtalk_load(Name, [hook(debug_expansion(production))]),
	load_interpreters(Is).

:- initialization((
	Interpreters = [dfs_interpreter - rule_expansion(production),
					bfs_interpreter - rule_expansion(production),
					iddfs_interpreter(_Inc) - rule_expansion(production),
					bup_interpreter - magic_expansion(production),
					greedy_best_first_interpreter - heuristic_expansion(production),
					a_star_interpreter - heuristic_expansion(production),
					a_star_interpreter_weighted - heuristic_expansion(production),
					a_star_interpreter_weighted2 - heuristic_expansion(production)],
	logtalk_load(library(all_loader)),
	logtalk_load(heap_loader),
	logtalk_load(counter_loader),
	logtalk_load(magic),
	logtalk_load(flatting),
	logtalk_load(debug_expansion),
	logtalk_load(rule_expansion),
	logtalk_load(magic_expansion),
	logtalk_load(shell_expansion),
	logtalk_load(heuristic_expansion),
	logtalk_load(benchmark_generators),
	logtalk_load(database, [hook(rule_expansion(production))]),
	logtalk_load(interpreterp),
	logtalk_load(best_first),
	pairs::keys(Interpreters, Interpreters1),
	write(Interpreters1),
	load_interpreters(Interpreters1),
	logtalk_load(shell, [hook(debug_expansion(production))]),
	shell(Interpreters)::init)).
