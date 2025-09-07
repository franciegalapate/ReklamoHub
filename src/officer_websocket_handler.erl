%% WebSocket handler for the officer dashboard.
%% It handles incoming messages and sends real-time updates.

-module(officer_websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% The initial handler. This function upgrades the HTTP connection to a WebSocket.
init(Req, State) ->
  {cowboy_websocket, Req, State}.

%% This is called once the connection has been upgraded.
websocket_init(State) ->
  io:format("~nWebSocket connection established for officer dashboard.~n"),
  {ok, State}.

%% This is called for every frame received from the client.
websocket_handle({text, Msg}, State) ->
  io:format("~nReceived message from officer dashboard: ~p~n", [Msg]),
  {reply, {text, Msg}, State};

websocket_handle(_Frame, State) ->
  {ok, State}.

%% This is called for every Erlang message received.
websocket_info(Info, State) ->
  io:format("~nReceived info message for officer dashboard: ~p~n", [Info]),

  % can add a case statement here to handle different message types.
  case Info of
       {new_complaint, ComplaintDetails} ->
           Reply = iolist_to_binary(jsx:encode(ComplaintDetails)),
           {reply, {text, Reply}, State};
       _ ->
          {ok, State}
   end,
  {ok, State}.

%% called when the connection is terminated.
terminate(_Reason, _Req, _State) ->
  io:format("~nWebSocket connection terminated.~n"),
  ok.
