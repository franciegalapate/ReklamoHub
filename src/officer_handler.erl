%% HTTP handler for the officer login.
%% It handles authentication before allowing access to the dashboard.

-module(officer_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  case Method of
    <<"POST">> ->
      handle_post(Req1, State);
    _ ->
      handle_other(Req1, State)
  end.

handle_post(Req, State) ->
  case cowboy_req:body_qs(Req) of
    {ok, {ok, Qs}, _} ->
      Username = proplists:get_value(<<"username">>, Qs, <<>>),
      Password = proplists:get_value(<<"password">>, Qs, <<>>),

      case db:authenticate_officer(Username, Password) of
        {ok, UserID} ->
          %% Successful login. Create a session token.
          Token = erlang:term_to_binary({session, UserID, erlang:timestamp()}),

          %% Redirect to the dashboard page with the token in a cookie.
          Req2 = cowboy_req:set_resp_cookie(<<"session_token">>, Token, Req),
          Req3 = cowboy_req:reply(302, #{<<"location">> => <<"/officer_dashboard.html">>}, Req2),
          {ok, Req3, State};
        {error, Reason} ->
          io:format("~nAuthentication failed: ~p~n", [Reason]),
          Req2 = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>}, <<"Authentication Failed">>, Req),
          {ok, Req2, State}
      end;
    _ ->
      Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Bad request">>, Req),
      {ok, Req2, State}
  end.

handle_other(Req, State) ->
  Req1 = cowboy_req:reply(405, Req),
  {ok, Req1, State}.
