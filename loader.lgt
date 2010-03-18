:- initialization((
    logtalk_load(rule_expansion),
	logtalk_load(interpreterp),
	logtalk_load(dfs_interpreter),
	logtalk_load(database, [hook(rule_expansion)]))).
