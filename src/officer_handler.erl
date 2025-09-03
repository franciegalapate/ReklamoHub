% Officer actions (login/dashboard/update) || Handles officer dashboard & actions
% DUMMY CODE

-module(officer_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Placeholder response for officers
    Json = <<"[{\"id\":1,\"category\":\"Noise\",\"status\":\"Pending\"}]">>,
    Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json">>},
            Json, Req0),
    {ok, Req, State}.