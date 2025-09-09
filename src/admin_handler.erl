-module(admin_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    case {Method, Path} of
        % Login page
        {<<"GET">>, <<"/admin_login">>} ->
            case file:read_file("priv/html/admin_login.html") of
                {ok, Html} ->
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"text/html">>}, Html, Req0),
                    {ok, Req, State};
                {error, Reason} ->
                    io:format("Error reading admin_login.html: ~p~n", [Reason]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Internal Server Error: Missing file.">>, Req0),
                    {ok, Req, State}
            end;

        % Login submit
        {<<"POST">>, <<"/admin_login">>} ->
            {ok, BodyBin, Req1} = cowboy_req:read_body(Req0),
            Params   = cow_qs:parse_qs(BodyBin),
            Username = qs(<<"username">>, Params),
            Password = qs(<<"password">>, Params),
            case check_login(Username, Password) of
                true ->
                    Headers = #{
                        <<"location">> => <<"/admin_dashboard">>,
                        <<"set-cookie">> => [<<"admin=1; Path=/; HttpOnly">>]
                    },
                    Req = cowboy_req:reply(302, Headers, <<>>, Req1),
                    {ok, Req, State};
                false ->
                    Req = cowboy_req:reply(
                        302,
                        #{<<"location">> => <<"/admin_login?error=1">>},
                        <<>>, Req1),
                    {ok, Req, State}
            end;

        % Dashboard
        {<<"GET">>, <<"/admin_dashboard">>} ->
            Cookies = cowboy_req:parse_cookies(Req0),
            case lists:keyfind(<<"admin">>, 1, Cookies) of
                {_, <<"1">>} ->
                    case file:read_file("priv/html/admin_dashboard.html") of
                        {ok, Html} ->
                            Req = cowboy_req:reply(200,
                                #{<<"content-type">> => <<"text/html">>}, Html, Req0),
                            {ok, Req, State};
                        {error, Reason} ->
                            io:format("Error reading admin_dashboard.html: ~p~n", [Reason]),
                            Req = cowboy_req:reply(500,
                                #{<<"content-type">> => <<"text/plain">>},
                                <<"Internal Server Error: Missing file.">>, Req0),
                            {ok, Req, State}
                    end;
                _ ->
                    Req = cowboy_req:reply(302,
                        #{<<"location">> => <<"/admin_login">>}, <<>>, Req0),
                    {ok, Req, State}
            end;

        % Dashboard API to get all complaints
        {<<"GET">>, <<"/api/complaints">>} ->
            case reklamohub_db:get_all_complaints() of
                {ok, #{columns := Cols, rows := Rows}} ->
                    JsonRows = [maps:from_list(lists:zip(Cols, Row)) || Row <- Rows],
                    Json = jsx:encode(JsonRows),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Json, Req0),
                    {ok, Req, State};
                {ok, Cols, Rows} ->
                    JsonRows = [maps:from_list(lists:zip(Cols, Row)) || Row <- Rows],
                    Json = jsx:encode(JsonRows),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Json, Req0),
                    {ok, Req, State};
                Error ->
                    io:format("❌ ERROR in /api/complaints: ~p~n", [Error]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"Database error\"}">>, Req0),
                    {ok, Req, State}
            end;

        % Update complaint status API
        {<<"POST">>, <<"/update_status">>} ->
            {ok, BodyBin, Req1} = cowboy_req:read_body(Req0),
            Map = jsx:decode(BodyBin, [return_maps]),
            ComplaintIDBin = maps:get(<<"complaint_id">>, Map),
            NewStatus      = maps:get(<<"status">>, Map),

            % "CMP-0005" -> 5
            ComplaintID =
                case ComplaintIDBin of
                    <<"CMP-", NumBin/binary>> -> list_to_integer(binary_to_list(NumBin));
                    NumBin -> list_to_integer(binary_to_list(NumBin))
                end,

            Sql = "UPDATE complaints SET status = ? WHERE complaint_id = ?",
            case db_manager:query(Sql, [NewStatus, ComplaintID]) of
                ok ->
                    % Fetch updated row from view (with CMP- prefix)
                    case reklamohub_db:get_complaint_by_id(ComplaintID) of
                        {ok, #{columns := Cols, rows := [Row]}} ->
                            JsonRow = maps:from_list(lists:zip(Cols, Row)),

                            % Call dashboard_manager to broadcast the status update
                            dashboard_manager:status_update(JsonRow),

                            Json = jsx:encode(JsonRow),
                            Req = cowboy_req:reply(200,
                                #{<<"content-type">> => <<"application/json">>},
                                Json, Req1),
                            {ok, Req, State};
                        {ok, Cols, [Row]} ->
                            JsonRow = maps:from_list(lists:zip(Cols, Row)),

                    
                            dashboard_manager:status_update(JsonRow),

                            Json = jsx:encode(JsonRow),
                            Req = cowboy_req:reply(200,
                                #{<<"content-type">> => <<"application/json">>},
                                Json, Req1),
                            {ok, Req, State};
                        FetchError ->
                            io:format("❌ ERROR fetching updated complaint: ~p~n", [FetchError]),
                            Req = cowboy_req:reply(500,
                                #{<<"content-type">> => <<"application/json">>},
                                <<"{\"error\":\"Status updated, but could not fetch complaint\"}">>, Req1),
                            {ok, Req, State}
                    end;
                Error ->
                    io:format("❌ ERROR in update_status query: ~p~n", [Error]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"Failed to update status\"}">>, Req1),
                    {ok, Req, State}
            end;
        _ ->
            {ok, Req0, State}
    end.

% Helper to get value from parsed query string

qs(Key, Params) ->
    case lists:keyfind(Key, 1, Params) of
        {_, V} -> V;
        false  -> <<>>
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L)   -> list_to_binary(L);
to_bin(Other)               -> iolist_to_binary(io_lib:format("~p", [Other])).

check_login(U, P) when U =:= <<>>; P =:= <<>> -> false;
check_login(U, P) ->
    case reklamohub_db:get_admin_password(U) of
        {ok, _Cols, [[DbPass]]} -> to_bin(P) =:= to_bin(DbPass);
        _ -> false
    end.