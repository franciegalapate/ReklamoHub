% Supervisor (Cowboy + DB connection)

-module(reklamohub_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("~n🚀 ReklamoHub starting...~n"),
    io:format("🌐 Cowboy will listen on http://localhost:8080~n~n"),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/complaints", complaint_handler, []},
            {"/api/officers", officer_handler, []},
            {"/[...]", reklamohub_router, []}
        ]}
    ]),

    {ok, {{one_for_one, 5, 10}, [
        {http_listener,
         {cowboy, start_clear, [
             http_listener,
             [{port, 8080}],
             #{env => #{dispatch => Dispatch}}
         ]},
         permanent, 5000, worker, dynamic}
    ]}}.