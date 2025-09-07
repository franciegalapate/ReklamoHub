-module(admin_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    case {Method, Path} of
        %% --------- LOGIN PAGE ----------
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

        %% --------- LOGIN SUBMIT ----------
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
                        #{<<"location">> => <<"/admin_login.html?error=1">>},
                        <<>>, Req1),
                    {ok, Req, State}
            end;

        %% --------- DASHBOARD HTML ----------
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

        %% --------- DASHBOARD DATA API ----------
        {<<"GET">>, <<"/api/complaints">>} ->
            case reklamohub_db:get_all_complaints() of
                {ok, #{columns := Cols, rows := Rows}} ->
                    JsonRows = [maps:from_list(lists:zip(Cols, Row)) || Row <- Rows],
                    Json = jsx:encode(JsonRows),
                    Req = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            Json, Req0),
                    {ok, Req, State};
                _ ->
                    Req = cowboy_req:reply(500,
                            #{<<"content-type">> => <<"application/json">>},
                            <<"{\"error\":\"Database error\"}">>, Req0),
                    {ok, Req, State}
            end;

        _ ->
            {ok, Req0, State}
    end.

%% ---------- Helpers ----------

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