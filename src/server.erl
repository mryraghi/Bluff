%%%-------------------------------------------------------------------
%%% @author Romeo
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2016 18:57
%%%-------------------------------------------------------------------
-module(server).
-author("Romeo").

%% API
-export([start/0, init/0, enter/2, terminate/0, flow/0, leave/1, ready/1]).

%% the server will need to keep two things in its state:
%%    a list of subscribing players
%%    a list of all the event processes it spawned

-record(state, {players, ready, deck}).

start() ->
  %% the atom ?MODULE can be used instead of the Pid
  register(?MODULE, Pid = spawn(?MODULE, init, [])),

  %% make the atom 'listen' available everywhere in this document
  register(flow, spawn(?MODULE, flow, [])),

  %% run 'init'
  Pid,

  %% auto compile me.erl
  compile:file(game),

  io:format("Server started!~nListening for reactions...~n"),
  spawn(?MODULE, flow(), []).

%%% GAME'S FLOW
flow() ->
  receive
    {enter, PlayerName, No_Players} ->
      io:format("-->  ~p has entered the game! (~p players [Min: 2, Max: 10]).~n", [PlayerName, No_Players]),
      flow();
    {leave, PlayerName, No_Players} ->
      io:format("-->  ~p has left the game! (~p players [Min: 2, Max: 10]).~n", [PlayerName, No_Players]),
      flow();
    {ready, ClientPid} ->
      io:format("==>  ~p is ready to play!~n", [ClientPid])
  end.

terminate() ->
  ?MODULE ! shutdown.

init() ->
  %% create and shuffle deck
  Deck = shuffle(make_deck()),

  %% init orddicts
  loop(#state{players = orddict:new(), ready = orddict:new(), deck = Deck}).

%%% THE SERVER
loop(S = #state{}) ->
  receive
    {ServerPid, Ref, {enter, PlayerName, PlayerPid}} ->
      %% check if # of players =< 10
      case orddict:size(S#state.players) + 1 =< 10 of
        true ->
          %% check if player's name already exists
          case orddict:find(PlayerPid, S#state.players) of
            {ok, _} ->
              %% if YES throw an error
              ServerPid ! {Ref, error_1},

              %% rerun loop(S) with no changes applied
              S#state.players,
              loop(S);
            error ->
              %% if NO store the player
              NewPlayers = orddict:store(PlayerPid, [PlayerName, []], S#state.players),

              %% send confirmation of creation
              ServerPid ! {Ref, ok, orddict:size(NewPlayers)},

              %% display success message on server
              flow ! {enter, PlayerName, orddict:size(NewPlayers)},

              %% rerun loop(S) with changes applied
              loop(S#state{players = NewPlayers})
          end;

        %% if # of players > 10 throw an error
        false -> ServerPid ! {Ref, error_2}
      end;

    {ServerPid, MsgRef, {leave, PlayerPid}} ->
      %% check if 'PlayerName' is in dictionary
      case orddict:find(PlayerPid, S#state.players) of
        %% if YES erase
        {ok, Player} ->
          %% erase player
          NewPlayers = orddict:erase(PlayerPid, S#state.players),

          %% send success message
          ServerPid ! {MsgRef, ok, lists:nth(1, Player)},

          %% display success message on server
          flow ! {leave, lists:nth(1, Player), orddict:size(NewPlayers)},

          %% rerun loop(S) with changes
          loop(S#state{players = NewPlayers});
        %% if NO throw an error
        error ->
          ServerPid ! {MsgRef, error},
          loop(S)
      end;

    {ServerPid, MsgRef, {ready, PlayerPid}} ->
      case orddict:find(PlayerPid, S#state.players) of
        {ok, _} ->
          %% display ready message on server
          %% flow ! {ready, Player},
          case orddict:find(PlayerPid, S#state.ready) of
            {ok, _} -> ServerPid ! {MsgRef, alreadyReady};
            error ->
              NewReadyPlayers = orddict:store(PlayerPid, MsgRef, S#state.ready),
              case orddict:size(NewReadyPlayers) == orddict:size(S#state.players) of
                true ->

%%                  N = ceiling(52 / orddict:size(NewReadyPlayers)),
%%                  Chunks = split(S#state.deck, N),
%%                  %% show_deck(Chunks),
%%
%%                  laed
%%
%%                  orddict:map(fun(K, _) ->
%%                    K ! {deck, lists:nth(1, Chunks)},
%%                    Chuck2 = lists:delete(1, Chunks),
%%                    Chunks = Chuck2
%%                              end, NewReadyPlayers),

%%
%%
%%                  List = [],
%%                  orddict:map(fun(K, X) ->
%%                    List = [_, X]
%%                              end, NewReadyPlayers),
%%
%%
%%
%%                  orddict:map(fun(K, V) ->
%%                    lists:zipwith(fun(X, Y) -> X + Y end, Chunks, lists:nth(2, V))
%%                    end, NewReadyPlayers),

                  loop(S#state{ready = orddict:new()});
                false ->
                  loop(S#state{ready = NewReadyPlayers})
              end
          end,

          loop(S);
        error ->
          ServerPid ! {MsgRef, notInTheGame},
          loop(S)
      end;

    shutdown ->
      exit(shutdown);

    {_, Ref, _, ClientPid, _} ->
      case orddict:find(ClientPid, S#state.players) of
        {ok, _} -> ?MODULE ! {self(), Ref, {leave, ClientPid}};
        error -> io:format("Error!")
      end,
      loop(S);

    {_Ref, _Message} ->
      loop(S);

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.

enter(PlayerName, PlayerPid) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {enter, PlayerName, PlayerPid}},
  receive
    {Ref, ok, No_Players} -> PlayerPid ! {ok, No_Players};
    {Ref, error_1} -> PlayerPid ! error_1;
    {Ref, error_2} -> PlayerPid ! error_2;
  %% if the server unexpectedly shuts down receive an error
    {'DOWN', Ref, process, _Pid, Reason} ->
      PlayerPid ! {error, Reason}
  after 5000 ->
    {error, timeout}
  end.


leave(PlayerPid) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {leave, PlayerPid}},
  receive
    {Ref, ok} -> PlayerPid ! ok;
    {Ref, error} -> PlayerPid ! error
  after 5000 ->
    {error, timeout}
  end.

ready(PlayerPid) ->

  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {ready, PlayerPid}},
  receive
    {Ref, gameStarts, PlayerDeck} -> PlayerPid ! {gameStarts, PlayerDeck};
    {Ref, alreadyReady} -> PlayerPid ! alreadyReady;
    {Ref, notInTheGame} -> PlayerPid ! notInTheGame;
    {Ref, deck} -> PlayerPid ! deck;
    {Ref, error, Reason} -> io:format("~p~n", [Reason])
  after 5000 ->
    {error, timeout}
  end.

make_deck() ->
  [{Value, Suit} || Value <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
    Suit <- ["Clubs", "Diamonds", "Hearts", "Spades"]].

show_deck(Deck) ->
  lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

shuffle(List) -> shuffle(List, []).

%% ff the list is empty, return the accumulated value.
shuffle([], Acc) -> Acc;

shuffle(List, Acc) ->
  %% the list of cards is split into two parts by a random card which is added
  %% into the 'Accumulator'; the two parts are then merged together and 'shuffle'
  %% is called again until the deck is empty

  %% random:uniform(N) => returns a random integer uniformly distributed between 1 and N
  %% split(N, List1) -> {List2, List3} => List2 contains the first N elements and List3 the rest
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).

part(List) ->
  part(List, []).
part([], Acc) ->
  lists:reverse(Acc);
part([H], Acc) ->
  lists:reverse([[H] | Acc]);
part([H1, H2 | T], Acc) ->
  part(T, [[H1, H2] | Acc]).

%% split lists into N parts
split([], _) -> [];
split(L, N) when length(L) < N -> [L];
split(L, N) ->
  {A, B} = lists:split(N, L),
  [A | split(B, N)].

floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T - 1
  end;
floor(X) ->
  trunc(X).


ceiling(X) when X < 0 ->
  trunc(X);
ceiling(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.