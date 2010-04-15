:- initialization((
    Interpreters = [dfs_interpreter - rule_expansion,
                    bfs_interpreter - rule_expansion,
                    iddfs_interpreter - rule_expansion],
    logtalk_load(library(all_loader)),
    logtalk_load(rule_expansion),
    logtalk_load(shell_expansion),
    logtalk_load(interpreterp),
    %%SWI Prolog specific.
    pairs_keys(Interpreters, Interpreters1),
    logtalk_load(Interpreters1),
    logtalk_load(shell, [hook(shell_expansion)]),
    shell(Interpreters)::init)).
