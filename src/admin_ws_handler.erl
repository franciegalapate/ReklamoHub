-module(admin_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

init(Req, State) ->
  {cowboy_websocket, Req, State, #{idle_timeout => 60000}}.

websocket_init(State) ->
  dashboard_manager:add_subscriber(self()),
  io:format("🌐 WS init: ~p~n", [self()]),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  try
    Map = jsx:decode(Msg, [return_maps]),
    case maps:get(<<"type">>, Map, undefined) of
      <<"ping">> ->
        io:format("Ping message received from client.~n"),
        %% You can reply with a pong here if you want, but Cowboy's built-in ping-pong is more efficient.
        %% The client-side ping is what matters to keep the connection alive.
        {ok, State};
      _ ->
        %% Handle other text messages here if needed
        {ok, State}
    end
  catch
    _Class:_Reason ->
      %% Message was not valid JSON, ignore or log it
      io:format("Received invalid message: ~p~n", [Msg]),
      {ok, State}
  end;

websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({complaint_update, Complaint}, State) ->
  io:format("📩 WS got complaint_update: ~p~n", [Complaint]),
  try
    Json = jsx:encode(#{<<"type">> => <<"update">>, <<"complaints">> => [Complaint]}),
    io:format("📤 WS send to client~n"),
    {reply, {text, Json}, State}
  catch
    Class:Reason:Stack ->
      io:format("❌ WS encode crash: ~p ~p ~p~n", [Class, Reason, Stack]),
      {ok, State}
  end;

websocket_info({new_complaint, Complaint}, State) ->
  io:format("📩 WS got new_complaint: ~p~n", [Complaint]),
  try
    Json = jsx:encode(#{<<"type">> => <<"new">>, <<"complaint">> => Complaint}),
    io:format("📤 WS send to client~n"),
    {reply, {text, Json}, State}
  catch
    Class:Reason:Stack ->
      io:format("❌ WS encode crash: ~p ~p ~p~n", [Class, Reason, Stack]),
      {ok, State}
  end;

websocket_info(_Info, State) ->
  io:format("ℹ️ WS got unknown info: ~p~n", [_Info]),
  {ok, State}.

websocket_terminate(_Reason, _Req, _State) ->
  dashboard_manager:remove_subscriber(self()),
  io:format("❌ WS terminate: ~p~n", [_Reason]),
  ok.