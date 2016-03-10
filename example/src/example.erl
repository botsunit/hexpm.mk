-module(example).

-export([one/0, one/1, two/2, three/3]).

one() ->
  echo("hello world").

one(hello) ->
  echo("hello world");
one(bonjour) ->
  echo("Bonjour le monde");
one(hola) ->
  echo("Hola mundo").

two(A, B) ->
  io:format("A = ~p, B = ~p~n", [A, B]).

three(A, B, C) ->
  io:format("A = ~p, B = ~p, C = ~p~n", [A, B, C]).

echo(X) ->
  io:format("~s~n", [X]).

