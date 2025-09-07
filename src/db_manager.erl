% Abstraction over MySQL connection. Talks directly to MySQL using mysql-otp.

-module(db_manager).
-export([start/0, stop/0, query/1, query/2]).

start() ->
    {ok, _} = application:ensure_all_started(mysql),
    PoolId = db_config:get_pool_id(),
    Config = db_config:get_db_config(),
    %% Wrap the name properly for gen_server registration:
    FixedName = {name, {local, PoolId}},
    %% (Optional) prefer "" over [] for password in Config
    Options = [FixedName | maps:to_list(Config)],
    case mysql:start_link(Options) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Error -> Error
    end.
    
stop() ->
    PoolId = db_config:get_pool_id(),
    mysql:stop(PoolId).

query(Sql) ->
    PoolId = db_config:get_pool_id(),
    mysql:query(PoolId, Sql).

query(Sql, Params) ->
    PoolId = db_config:get_pool_id(),
    mysql:query(PoolId, Sql, Params).