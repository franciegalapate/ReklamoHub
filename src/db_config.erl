% Stores connection info (pool name, host, port, user, etc.).
% Called by db_manager.

-module(db_config).
-export([get_pool_id/0, get_db_config/0]).

get_pool_id() ->
    complaint_pool.

get_db_config() ->
    #{
        host => "localhost",
        port => 3306,
        user => "root",
        password => "",
        database => "reklamohub_db",
        encoding => utf8,
        pool_size => 5
    }.