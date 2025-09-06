% Main application (starts Cowboy)

-module(reklamohub_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    io:format("🔗 Connecting to MySQL...~n"),
    case db_manager:start() of
        {ok, _Pid} ->
            io:format("MySQL connection established~n");
        {error, Reason} ->
            io:format("Failed to connect to MySQL: ~p~n", [Reason])
    end,
    reklamohub_sup:start_link().

stop(_State) ->
    db_manager:stop(),
    io:format("ReklamoHub stopping...~n"),
    ok.