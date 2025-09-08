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
        {ok, State};
      _ ->
        {ok, State}
    end
  catch
    _Class:_Reason ->
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

websocket_info({complaint_status_update, Complaint}, State) ->
  TS = os:timestamp(), % or erlang:now(), they both return {MegaSecs, Secs, MicroSecs}
  {_,_,Micro} = TS,
  {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time(TS),
  io:format("~p: 📩 WS got complaint_status_update: ~p~n",
    [[{Year, Month, Day}, {Hour, Minute, Second}, Micro], Complaint]),
  try
    Json = jsx:encode(#{
      <<"type">> => <<"status_update">>,
      <<"complaint">> => Complaint
    }),
    io:format("~p: 📤 WS sending status update to client~n", [[{Year, Month, Day}, {Hour, Minute, Second}, Micro]]),
    {reply, {text, Json}, State}
  catch
    Class:Reason:Stack ->
      io:format("~p: ❌ WS encode crash: ~p ~p ~p~n", [[{Year, Month, Day}, {Hour, Minute, Second}, Micro], Class, Reason, Stack]),
      {ok, State}
  end;

websocket_info(_Info, State) ->
  io:format("ℹ️ WS got unknown info: ~p~n", [_Info]),
  {ok, State}.

websocket_terminate(_Reason, _Req, _State) ->
  dashboard_manager:remove_subscriber(self()),
  io:format("❌ WS terminate: ~p~n", [_Reason]),
  ok.