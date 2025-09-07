% The business logic for handling complaints (submitting, tracking, retrieving status).
% Likely called from router when a request matches /complaint or /track.

-module(complaint_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),

    case {Method, Path} of
        {<<"POST">>, <<"/submit_complaint">>} ->
            handle_submit(Req0, State);

        {<<"GET">>, <<"/track_complaint">>} ->
            handle_track(Req0, State);

        _ ->
            Req = cowboy_req:reply(404, #{}, <<"Not found">>, Req0),
            {ok, Req, State}
    end.

% Submit a new complaint
handle_submit(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    {ok, Map} = jsx:decode(Body, [return_maps]),

    Resident = maps:get(<<"resident">>, Map, null),
    Category = maps:get(<<"category">>, Map),
    Address  = maps:get(<<"address">>, Map),
    Details  = maps:get(<<"details">>, Map),
    Img      = maps:get(<<"img">>, Map, null),

    case reklamohub_db:save_complaint(Resident, Category, Address, Details, Img) of
        {ok, #{last_insert_id := ID}} ->
            Json = jsx:encode(#{ message => <<"Complaint submitted">>, complaint_id => ID }),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Json, Req1),
            {ok, Req, State};

        _Error ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Could not save complaint\"}">>, Req1),
            {ok, Req, State}
    end.

% Track complaint by ID
handle_track(Req0, State) ->
    {ComplaintIDStr, Req1} = cowboy_req:qs_val(<<"id">>, Req0),
    ComplaintID = list_to_integer(binary_to_list(ComplaintIDStr)),

    case reklamohub_db:get_complaint_by_id(ComplaintID) of
        {ok, #{rows := []}} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Complaint not found\"}">>, Req1),
            {ok, Req, State};

        {ok, #{columns := Cols, rows := [Row]}} ->
            Obj = maps:from_list(lists:zip(Cols, Row)),
            Json = jsx:encode(Obj),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Json, Req1),
            {ok, Req, State};

        _Error ->
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Database error\"}">>, Req1),
            {ok, Req, State}
    end.