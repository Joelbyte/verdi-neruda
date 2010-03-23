:- initialization((
    logtalk_load([library(metap), library(meta)]),
    logtalk_load(rule_expansion),
    logtalk_load(interpreterp),
    logtalk_load([dfs_interpreter, bfs_interpreter]),
    logtalk_load(shell),
    logtalk_load(database, [hook(rule_expansion)]))).
