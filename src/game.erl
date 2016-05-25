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
-export([enter/1, leave/0, ready/0, play/1, bluff/0, cards/0]).


enter(PlayerName) ->
  server:enter(PlayerName, self()),
  receive
    {ok, Deck} ->
      io:format("-->  You have entered the game!~nYour cards:~n"),
      printList(1, Deck, []);
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
  io:format("Waiting for opponents...~n"),
  server:ready(self()),
  receive
    {deck, Deck} ->
      io:format("Your cards:~n~p~n", [Deck]),
      ready();

    isYourTurn ->
      io:format("Is your turn!~n");
    isNotYourTurn ->
      io:format("Wait, is not your turn! Check the server.~n");

    play ->
      io:format("-->  Is your turn!~n");
    ok ->
      io:format("-->  Everybody is ready!~n");
    notInTheGame ->
      io:format("-->  You are not in the game!~n");
    alreadyReady ->
      io:format("-->  You are already ready!~n")
  end.

play(Card) ->
  server:play(self(), Card),
  receive
    ok ->
      io:format("");
    {endTurn, NewDeck} ->
      io:format("You played!~nYour cards:~n~p~n", [NewDeck]);
    error ->
      io:format("You are not in the game.~n");
    error_1 ->
      io:format("Is not your turn!~n~n");
    error_2 ->
      io:format("You played the wrong card!~n");
    error_3 ->
      io:format("You are playing too many cards!~n")
  end.

bluff() ->
  server:bluff(self()),
  receive
    ok ->
      io:format("");
    isNotBluff ->
      io:format("You are wrong, the last player didn't bluff.~n");
    isBluff ->
      io:format("You are right! The last player bluffed.~n");
    notInTheGame ->
      io:format("-->  You are not in the game!~n")
  end.

cards() ->
  server:cards(self()),
  receive
    {cards, Cards} ->
      io:format("Your cards:~n"),
      printList(1, Cards, []);
    notInTheGame ->
      io:format("-->  You are not in the game!~n")
  end.

printList(_, [], _) -> ok;
printList(Counter, List, Acc) ->
  {H, T} = lists:split(1, List),
  io:format("~p --> ~p~n", [Counter, H]),
  NewAcc = [H, Acc],
  printList(Counter + 1, T, NewAcc).