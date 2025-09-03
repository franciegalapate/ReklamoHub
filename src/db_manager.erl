-module(db_manager).
-export([start/0, stop/0, query/2, query/3]).

start() ->
    PoolId = db_config:get_pool_id(),
    Config = db_config:get_db_config(),
    mysql:start_link(PoolId, maps:to_list(Config)).

stop() ->
    PoolId = db_config:get_pool_id(),
    mysql:stop(PoolId).

query(Sql, Params) ->
    PoolId = db_config:get_pool_id(),
    mysql:query(PoolId, Sql, Params).

query(Sql) ->
    PoolId = db_config:get_pool_id(),
    mysql:query(PoolId, Sql).