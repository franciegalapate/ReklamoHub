% Supervisor (Cowboy + DB connection)
% Supervisor for fault tolerance. Ensures that db_manager and other workers restart if they crash.

-module(reklamohub_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
init([]) ->
    io:format("~nReklamoHub starting...~n"),

    DbChild =
        {db_manager,
         {db_manager, start, []},
         permanent, 5000, worker, [db_manager]},

    %% --- Cowboy routes ---
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/submit_complaint", complaint_handler, []},
            {"/track_complaint", complaint_handler, []},
            {"/admin_login", admin_handler, []},
            {"/admin_dashboard", admin_handler, []},
            {"/update_status", admin_handler, []},
            {"/api/complaints", admin_handler, []},
            {"/[...]", reklamohub_router, []}
        ]}
    ]),

    %% --- Cowboy listener ---
    CowboyChild =
        {http_listener,
         {cowboy, start_clear, [
             http_listener,
             [{port, 8080}],
             #{env => #{dispatch => Dispatch}}
         ]},
         permanent, 5000, worker, dynamic},

    {ok, {{one_for_one, 5, 10}, [DbChild, CowboyChild]}}.