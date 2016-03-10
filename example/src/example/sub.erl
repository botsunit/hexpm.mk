-module(sub).

-export([welcome/0]).

welcome() ->
  io:format("Welcome, Alice!~n").

