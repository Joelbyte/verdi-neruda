:- initialization((
	Interpreters = [dfs_interpreter - rule_expansion,
					bfs_interpreter - rule_expansion,
					iddfs_interpreter - rule_expansion,
					bup_interpreter - magic_expansion,
					greedy_best_first_interpreter - heuristic_expansion,
					a_star_interpreter - heuristic_expansion,
					a_star_interpreter_weighted - heuristic_expansion,
					a_star_interpreter_weighted2 - heuristic_expansion],
	logtalk_load(library(all_loader)),
	logtalk_load(heap_loader),
	logtalk_load(flatting),
	logtalk_load(rule_expansion),
	logtalk_load(magic_expansion),
	logtalk_load(shell_expansion),
	logtalk_load(heuristic_expansion),
	logtalk_load(benchmark_generators),
	logtalk_load(database, [hook(rule_expansion)]),
	logtalk_load(interpreterp),
	logtalk_load(magic),
	logtalk_load(best_first),
	pairs::keys(Interpreters, Interpreters1),
	write(Interpreters1),
	logtalk_load(Interpreters1),
	logtalk_load(shell, [hook(shell_expansion)]),
	shell(Interpreters)::init)).
