% Main application (starts Cowboy)

-module(reklamohub_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    reklamohub_sup:start_link().

stop(_State) ->
    ok.