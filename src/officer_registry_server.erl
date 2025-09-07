%% This gen_server keeps a list of all connected admin WebSocket process IDs.
%% It is responsible for broadcasting messages to all of them.

-module(officer_registry_server).
-behaviour(gen_server).

-export([start_link/0, register_officer/1, unregister_officer/1, broadcast/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {officers = []}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_officer(Pid) ->
  gen_server:cast(?MODULE, {register, Pid}).

unregister_officer(Pid) ->
  gen_server:cast(?MODULE, {unregister, Pid}).

broadcast(Message) ->
  gen_server:cast(?MODULE, {broadcast, Message}).

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({register, Pid}, State = #state{officers = Officers}) ->
  io:format("~nRegistering new officer PID: ~p~n", [Pid]),
  NewOfficers = [Pid | Officers],
  {noreply, State#state{officers = NewOfficers}};

handle_cast({unregister, Pid}, State = #state{officers = Officers}) ->
  io:format("~nUnregistering officer PID: ~p~n", [Pid]),
  NewOfficers = lists:delete(Pid, Officers),
  {noreply, State#state{officers = NewOfficers}};

handle_cast({broadcast, Message}, State = #state{officers = Officers}) ->
  io:format("~nBroadcasting message to ~p officers: ~p~n", [length(Officers), Message]),
  [ Pid ! Message || Pid <- Officers ],
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
