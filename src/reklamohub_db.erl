% DB queries (insert, list, update)
% DUMMY CODE

-module(reklamohub_db).
-export([insert_complaint/1, list_complaints/0, update_complaint/2]).

%% Insert a complaint (dummy for now)
insert_complaint(Complaint) ->
    io:format("Inserting complaint: ~p~n", [Complaint]),
    {ok, 1}.  %% returns {ok, ComplaintID}

%% List all complaints (dummy)
list_complaints() ->
    [
        #{id => 1, category => <<"Noise">>, status => <<"Pending">>},
        #{id => 2, category => <<"Garbage">>, status => <<"Resolved">>}
    ].

%% Update complaint (dummy)
update_complaint(Id, NewStatus) ->
    io:format("Updating complaint ~p to ~p~n", [Id, NewStatus]),
    ok.
