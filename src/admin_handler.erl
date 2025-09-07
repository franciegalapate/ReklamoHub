% Minimal admin login using mysql-otp pool via db_manager

-module(admin_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/admin_login">>} ->
            Req = cowboy_req:reply(302, #{<<"location">> => <<"/admin_login.html">>}, <<>>, Req0),
            {ok, Req, State};

        {<<"POST">>, <<"/admin_login">>} ->
            {ok, BodyBin, Req1} = cowboy_req:read_body(Req0),
            Params   = cow_qs:parse_qs(BodyBin),
            Username = qs(<<"username">>, Params),
            Password = qs(<<"password">>, Params),
            case check_login(Username, Password) of
                true ->
                    Headers = #{
                        <<"set-cookie">> => <<"admin=1; HttpOnly; Path=/">>,
                        <<"location">>   => <<"/admin_dashboard">>
                    },
                    Req = cowboy_req:reply(302, Headers, <<>>, Req1),
                    {ok, Req, State};
                false ->
                    Req = cowboy_req:reply(302, #{<<"location">> => <<"/admin_login.html?error=1">>}, <<>>, Req1),
                    {ok, Req, State}
            end;

        {<<"GET">>, <<"/admin_dashboard">>} ->
            Cookies = cowboy_req:parse_cookies(Req0),
            case lists:keyfind(<<"admin">>, 1, Cookies) of
                {_, <<"1">>} ->
                    Req = cowboy_req:reply(302, #{<<"location">> => <<"/admin_dashboard.html">>}, <<>>, Req0),
                    {ok, Req, State};
                _ ->
                    Req = cowboy_req:reply(302, #{<<"location">> => <<"/admin_login.html">>}, <<>>, Req0),
                    {ok, Req, State}
            end;

        _ ->
            {ok, Req0, State}
    end.

%% Helpers

qs(Key, Params) ->
    case lists:keyfind(Key, 1, Params) of
        {_, V} -> V;
        false  -> <<>>
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L)   -> list_to_binary(L);
to_bin(Other)               -> iolist_to_binary(io_lib:format("~p", [Other])).

check_login(U, P) when U =:= <<>>; P =:= <<>> ->
    false;
check_login(U, P) ->
    case reklamohub_db:get_admin_password(U) of
        {ok, #{rows := [[DbPass]]}} ->
            to_bin(P) =:= to_bin(DbPass);
        _ ->
            false
    end.