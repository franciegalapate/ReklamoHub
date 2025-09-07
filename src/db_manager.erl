% Abstraction over MySQL connection. Talks directly to MySQL using mysql-otp.

-module(db_manager).
-export([start/0, stop/0, query/1, query/2]).

start() ->
    PoolId = db_config:get_pool_id(),
    Config = db_config:get_db_config(),
    Options = [{name, PoolId} | maps:to_list(Config)],
    mysql:start_link(Options).

stop() ->
    PoolId = db_config:get_pool_id(),
    mysql:stop(PoolId).

query(Sql) ->
    PoolId = db_config:get_pool_id(),
    mysql:query(PoolId, Sql).

query(Sql, Params) ->
    PoolId = db_config:get_pool_id(),
    mysql:query(PoolId, Sql, Params).