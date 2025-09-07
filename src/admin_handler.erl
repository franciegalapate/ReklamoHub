% Minimal admin login using mysql-otp pool via db_manager

-module(admin_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    case {Method, Path} of
        %% ---- Serve login page (redirect to static HTML) ----
        {<<"GET">>, <<"/admin_login">>} ->
            Req = cowboy_req:reply(
                    302,
                    #{<<"location">> => <<"/admin_login.html">>},
                    <<>>, Req0),
            {ok, Req, State};

        %% ---- Handle login submit ----
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
                    Req = cowboy_req:reply(
                            302,
                            #{<<"location">> => <<"/admin_login.html?error=1">>},
                            <<>>, Req1),
                    {ok, Req, State}
            end;

        %% ---- Protected dashboard gate ----
        {<<"GET">>, <<"/admin_dashboard">>} ->
            Cookies = cowboy_req:parse_cookies(Req0),
            case lists:keyfind(<<"admin">>, 1, Cookies) of
                {_, <<"1">>} ->
                    %% Auth OK -> hand off to static dashboard page
                    Req = cowboy_req:reply(
                            302,
                            #{<<"location">> => <<"/admin_dashboard.html">>},
                            <<>>, Req0),
                    {ok, Req, State};
                _ ->
                    %% Not logged in -> go back to login
                    Req = cowboy_req:reply(
                            302,
                            #{<<"location">> => <<"/admin_login.html">>},
                            <<>>, Req0),
                    {ok, Req, State}
            end;

        _ ->
            {ok, Req0, State}
    end.

%% --- helpers ---

qs(Key, Params) ->
    case lists:keyfind(Key, 1, Params) of
        {_, V} -> V;
        false  -> <<>>
    end.

check_login(U, P) when U =:= <<>>, P =:= <<>> -> false;
check_login(U, P) ->
    %% Parameterized query; your dump seeds ('admin','admin') for quick test.
    case db_manager:query(
           <<"SELECT admin_password FROM admin WHERE admin_username = ? LIMIT 1">>,
           [U]) of
        {ok, #{rows := [[DbPass]]}} ->
            %% plaintext compare because seed is plaintext; swap to hash later
            P =:= DbPass;
        _ ->
            false
    end.