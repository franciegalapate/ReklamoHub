-module(reklamoHub_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  %% Set up Cowboy routes
  Dispatch = cowboy_router:compile([
    {'_', [{"/", reklamoHub_handler, []}]}
  ]),

  %% Start Cowboy on port 8080
  {ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),

  io:format("ReklamoHub running at http://localhost:8080~n"),

  {ok, self()}.

stop(_State) ->
  ok.
