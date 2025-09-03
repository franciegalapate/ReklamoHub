% Resident complaints (submit/view) || Handles resident complaint submission
% Demo code only, PLEASE REPLACE

-module(complaint_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Placeholder: return static JSON
    Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json">>},
            <<"[{\"id\":1,\"text\":\"This is a test complaint\"}]">>,
            Req0),
    {ok, Req, State}.