:- initialization((
    Interpreters = [dfs_interpreter, bfs_interpreter],
    logtalk_load([library(metap), library(meta)]),
    logtalk_load(rule_expansion),
    logtalk_load(interpreterp),
    logtalk_load(Interpreters),
    logtalk_load(shell),
    logtalk_load(database, [hook(rule_expansion)]),
    shell(Interpreters)::init)).
