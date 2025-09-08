%% dashboard_manager.erl
%% Keeps track of websocket subscribers and broadcasts complaints.
-module(dashboard_manager).
-behaviour(gen_server).

%% Public API
-export([start_link/0, add_subscriber/1, new_complaint/1, status_update/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-record(state, {subscribers = #{}}).  %% map: Pid => MonitorRef

%%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Called by the websocket handler when a dashboard connects
add_subscriber(Pid) when is_pid(Pid) ->
  gen_server:cast(?MODULE, {add_subscriber, Pid}).

%% Called by complaint_handler when a new complaint is saved
new_complaint(Complaint) ->
  gen_server:cast(?MODULE, {broadcast_new, Complaint}).

%% Called by handlers when status changes
status_update(Complaint) ->
  gen_server:cast(?MODULE, {broadcast_status, Complaint}).

%%% Callbacks
init([]) ->
  {ok, #state{subscribers = #{}}}.

handle_cast({add_subscriber, Pid}, State = #state{subscribers = Subs}) ->
  Ref = erlang:monitor(process, Pid),
  io:format("📡 Dashboard subscribed: ~p (ref ~p)~n", [Pid, Ref]),
  NewSubs = maps:put(Pid, Ref, Subs),
  {noreply, State#state{subscribers = NewSubs}};

handle_cast({broadcast_new, Complaint}, State = #state{subscribers = Subs}) ->
  io:format("📢 Broadcasting NEW complaint to ~p dashboards~n", [maps:size(Subs)]),
  maps:foreach(fun(Pid, _Ref) -> Pid ! {complaint_update, Complaint} end, Subs),
  {noreply, State};

handle_cast({broadcast_status, Complaint}, State = #state{subscribers = Subs}) ->
  io:format("🔄 Broadcasting STATUS UPDATE to ~p dashboards~n", [maps:size(Subs)]),
  maps:foreach(fun(Pid, _Ref) -> Pid ! {complaint_status_update, Complaint} end, Subs),
  {noreply, State};

handle_cast(_Other, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% Remove subscriber automatically when process dies
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{subscribers = Subs}) ->
  io:format("❌ Dashboard disconnected: ~p~n", [Pid]),
  {noreply, State#state{subscribers = maps:remove(Pid, Subs)}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
