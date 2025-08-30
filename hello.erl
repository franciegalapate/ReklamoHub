-module(hello). % module name
-export([world/0, greet/1]). % function: world, args: 0

world() ->
    io:format("Hello, world!~n").

greet(Name) ->
    io:format("Hello, ~s!~n", [Name]).