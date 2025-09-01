%%%-------------------------------------------------------------------
%%% @author Nicole
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2025 4:47 PM
%%%-------------------------------------------------------------------
-module(reklamoHub_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
  %% Sample dynamic data
  Complaints = [
    #{user => <<"Alice">>, message => <<"complaint>},
    #{user => <<"Bob">>,   message => <<"complaint">>}
  ],

  %% Build HTML as iolist
  Items = [io_lib:format("<li>~s: ~s</li>", [maps:get(user,C), maps:get(message,C)]) || C <- Complaints],

  Body = [
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>ReklamoHub</title></head><body>",
    "<h1>Complaints</h1><ul>",
    Items,
    "</ul></body></html>"
  ],

  %% Send response
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/html; charset=utf-8">>},
    Body,
    Req0),

  {ok, Req, State}.

