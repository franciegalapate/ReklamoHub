-module(admin_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

init(Req, State) ->
  {cowboy_websocket, Req, State}.

websocket_init(State) ->
  %% Subscribe this socket process to dashboard_manager
  dashboard_manager:add_subscriber(self()),
  io:format("🌐 WS init: ~p~n", [self()]),
  {ok, State}.

websocket_handle({text, _Msg}, State) ->
  %% (Optional) Handle incoming messages from client
  {ok, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({complaint_update, Complaint}, State) ->
  try
    Json = jsx:encode(#{<<"type">> => <<"new">>, <<"complaints">> => [Complaint]}),
    io:format("📤 WS send NEW (safe encode): ~p~n", [Complaint]),
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
  io:format("❌ WS terminate: ~p~n", [_Reason]),
  ok.
