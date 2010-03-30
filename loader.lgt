:- initialization((
    Interpreters = [dfs_interpreter, bfs_interpreter],
    logtalk_load(library(all_loader)),
    logtalk_load(rule_expansion),
    logtalk_load(shell_expansion),
    logtalk_load(interpreterp),
    logtalk_load(Interpreters),
    logtalk_load(shell, [hook(shell_expansion)]),
    logtalk_load(database, [hook(rule_expansion)]),
    shell(Interpreters)::init)).
