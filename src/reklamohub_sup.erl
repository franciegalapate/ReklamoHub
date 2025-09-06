% Supervisor (Cowboy + DB connection)

-module(reklamohub_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("~nReklamoHub starting...~n"),

    Dispatch = cowboy_router:compile([
        {'_', [
            %% API routes
            {"/submit_complaint", complaint_handler, []},
            {"/track_complaint", complaint_handler, []},
            {"/admin_login", admin_handler, []},
            {"/admin_dashboard", admin_handler, []},
            {"/update_status", admin_handler, []},

            %% Static files (fallback)
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