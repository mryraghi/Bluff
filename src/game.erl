%%%-------------------------------------------------------------------
%%% @author Romeo
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2016 21:12
%%%-------------------------------------------------------------------
-module(game).
-author("Romeo").

%% API
-export([enter/1, leave/0, ready/0]).


enter(PlayerName) ->
  server:enter(PlayerName, self()),
  receive
    {ok, No_Players} ->
      io:format("-->  You have entered the game! (~p players [Min: 2, Max: 10]).~n", [No_Players]);
    error_1 ->
      io:format("-->  You've alerady entered the game!~n");
    error_2 ->
      io:format("Maximum number of players reached!~n");
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason])
  end.

leave() ->
  server:leave(self()),
  receive
    ok ->
      io:format("-->  You have left the game!.~n");
    error ->
      io:format("Either you've already exited the game or you've mistyped" ++
      "the username!~nType 'game:enter(\"USERNAME\").' to rejoin.~n")
  end.

ready() ->
  server:ready(self()),
  receive
    {deck, Deck} ->
      io:format("~p~n", [Deck]);
    play ->
      io:format("-->  Is your turn!~n");
    {gameStarts, PlayerDeck} ->
      io:format("-->  Everybody is ready!~n~p", [PlayerDeck]);
    notInTheGame ->
      io:format("-->  You are not in the game!~n");
    alreadyReady ->
      io:format("-->  You are already ready!~n")
  end.
