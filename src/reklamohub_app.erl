% Main application (starts Cowboy)
% Starts your supervisor (reklamohub_sup) on app boot.

-module(reklamohub_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    io:format("Starting ReklamoHub...~n"),
    reklamohub_sup:start_link().

stop(_State) ->
    io:format("ReklamoHub stopping...~n"),
    ok = cowboy:stop_listener(http_listener),
    io:format("ReklamoHub stopped cleanly.~n"),
    ok.