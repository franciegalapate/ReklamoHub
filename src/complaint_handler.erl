% The business logic for handling complaints (submitting, tracking, retrieving status).

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
    Map = jsx:decode(Body, [return_maps]),

    Resident = maps:get(<<"resident">>, Map, null),
    Category = maps:get(<<"category">>, Map),
    Address  = maps:get(<<"address">>, Map),
    Details  = maps:get(<<"details">>, Map),
    Img      = maps:get(<<"img">>, Map, null),

    case reklamohub_db:save_complaint(Resident, Category, Address, Details, Img) of
        {ok, #{last_insert_id := ID}} ->
            %% Fetch the full complaint row using same DB logic as tracking
            case reklamohub_db:get_complaint_by_id(ID) of
                {ok, #{columns := Cols, rows := [Row]}} ->
                    Complaint = normalize_complaint(maps:from_list(lists:zip(Cols, Row))),

                    %%Notify dashboard_manager
                    dashboard_manager:new_complaint(Complaint),

                    reply_with_complaint(Cols, Row, Req1, State);

                {ok, Cols, [Row]} ->
                    Complaint = normalize_complaint(maps:from_list(lists:zip(Cols, Row))),

                    %% Notify dashboard_manager
                    dashboard_manager:new_complaint(Complaint),

                    reply_with_complaint(Cols, Row, Req1, State);

                FetchError ->
                    io:format("❌ ERROR fetching complaint after insert: ~p~n", [FetchError]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"Complaint saved, but could not retrieve details\"}">>, Req1),
                    {ok, Req, State}
            end;

        SaveError ->
            io:format("❌ ERROR in save_complaint: ~p~n", [SaveError]),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Could not save complaint\"}">>, Req1),
            {ok, Req, State}
    end.

% Track complaint by ID
handle_track(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    ComplaintIDStr = proplists:get_value(<<"id">>, Qs),
    Req1 = Req0,

    %% Normalize input: allow "CMP-0026" or "26"
    ComplaintID =
        case ComplaintIDStr of
            <<"CMP-", NumBin/binary>> ->
                list_to_integer(binary_to_list(NumBin));
            NumBin ->
                list_to_integer(binary_to_list(NumBin))
        end,

    case reklamohub_db:get_complaint_by_id(ComplaintID) of
        {ok, #{rows := []}} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Complaint not found\"}">>, Req1),
            {ok, Req, State};

        {ok, #{columns := Cols, rows := [Row]}} ->
            reply_with_complaint(Cols, Row, Req1, State);
        {ok, Cols, [Row]} ->
            reply_with_complaint(Cols, Row, Req1, State);

        Error ->
            io:format("❌ ERROR in handle_track: ~p~n", [Error]),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Database error\"}">>, Req1),
            {ok, Req, State}
    end.

%% Helper to standardize the complaint response
reply_with_complaint(Cols, Row, Req, State) ->
    Obj0 = maps:from_list(lists:zip(Cols, Row)),
    Obj  = normalize_complaint(Obj0),
    Json = jsx:encode(Obj),
    Resp = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json, Req),
    {ok, Resp, State}.

normalize_complaint(Obj0) ->
    #{
        complaint_id => maps:get(<<"complaint_id">>, Obj0),
        resident     => maps:get(<<"resident">>, Obj0),
        category     => maps:get(<<"category">>, Obj0),
        status       => maps:get(<<"status">>, Obj0),
        date         => maps:get(<<"filed_date">>, Obj0),
        img          => maps:get(<<"img">>, Obj0),
        address      => maps:get(<<"address">>, Obj0),
        details      => maps:get(<<"details">>, Obj0)
    }.