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
-export([start/1, init/1, enter/2, terminate/0, flow/0, leave/1, ready/1, play/2, bluff/1, cards/1]).

%% the server will need to keep two things in its state:
%%    a list of subscribing players
%%    a list of all the event processes it spawned

-record(state, {players, ready, deck, turns, pile}).

start(No_Players) ->
  %% the atom ?MODULE can be used instead of the Pid
  register(?MODULE, Pid = spawn(?MODULE, init, [No_Players])),

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
    {play, PlayerName, Cards, NextPlayer} ->
      io:format("~n==>  ~p is playing ~p ~p!~n==>  Next player: ~p~n", [PlayerName, lists:nth(1, Cards), lists:nth(2, Cards), NextPlayer]),
      flow();
    {bluff, DeclaringPlayer, Player} ->
      io:format("~n==>  ~p says that ~p is bluffing!~n", [DeclaringPlayer, Player]),
      flow();
    {isBluff, BlufferPlayer} ->
      io:format("~n==>  He was right! ~p was bluffing! ~p takes all the cards.~nType game:cards(). to see your cards.~n", [BlufferPlayer, BlufferPlayer]),
      flow();
    {isNotBluff, DeclaringPlayer} ->
      io:format("~n==>  Mmh, nope. ~p wasn't right! He wasn't bluffing! ~p takes all the cards.~nType game:cards(). to see your cards.~n", [DeclaringPlayer, DeclaringPlayer]),
      flow();
    {win, Winner} ->
      io:format("~n==>  ~p has won!~n", [Winner])
  end.

terminate() ->
  ?MODULE ! shutdown.

init(No_Players) ->
  %% create and shuffle deck
  Deck = shuffle(make_deck()),

  N = ceiling(52 / No_Players),
  Chunks = split(Deck, N),

  %% init orddicts
  loop(#state{players = orddict:new(), ready = orddict:new(), deck = Chunks, turns = [], pile = []}).

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
              NewPlayers = orddict:store(PlayerPid, [PlayerName, lists:nth(1, S#state.deck)], S#state.players),

              %% add PlayerPid to turns
              Turns = [[PlayerPid, []] | S#state.turns],

              %% send confirmation of creation
              ServerPid ! {Ref, ok, lists:nth(1, S#state.deck)},

              %% display success message on server
              flow ! {enter, PlayerName, orddict:size(NewPlayers)},

              NewDeck = lists:nthtail(1, S#state.deck),

              %% rerun loop(S) with changes applied
              loop(S#state{players = NewPlayers, deck = NewDeck, turns = Turns})
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
          case orddict:find(PlayerPid, S#state.ready) of
            {ok, _} -> ServerPid ! {MsgRef, alreadyReady};
            error ->
              NewReadyPlayers = orddict:store(PlayerPid, MsgRef, S#state.ready),
              case orddict:size(NewReadyPlayers) == orddict:size(S#state.players) of
                true ->

                  CurrentTurn = lists:nth(1, lists:nth(1, S#state.turns)),

                  orddict:map(fun(K, _) ->
                    case CurrentTurn == K of
                      true -> K ! isYourTurn;
                      false -> K ! isNotYourTurn
                    end
                              end, S#state.players),


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

    {ServerPid, MsgRef, {bluff, PlayerPid}} ->
      case orddict:find(PlayerPid, S#state.players) of
        {ok, DeclaringPlayer} ->
          CurrentTurn = lists:nth(1, lists:nth(1, S#state.turns)),
          %% check if is player's turn
          case PlayerPid == CurrentTurn of
            true ->
              LastTurn = lists:nth(2, S#state.turns),
              LastCardsPlayer = lists:nth(1, lists:nth(2, LastTurn)),
              RankDeclared = lists:nth(2, lists:nth(2, LastTurn)),

              DictLastCardsPlayed = orddict:from_list(LastCardsPlayer),

              orddict:map(fun(_, V) ->
                case V /= RankDeclared of
                  true -> ServerPid ! {MsgRef, isBluff, lists:nth(1, LastTurn)},
                    loop(S);
                  false -> continue
                end
                          end, DictLastCardsPlayed),

              %% CHECK HERE
              case orddict:find(lists:nth(1, LastTurn), S#state.players) of
                {ok, Player} -> flow ! {bluff, lists:nth(1, DeclaringPlayer), lists:nth(1, Player)}
              end,

              ServerPid ! {MsgRef, isNotBluff, PlayerPid, lists:nth(1, LastTurn)},

              loop(S);
            false ->
              ServerPid ! {MsgRef, error},
              loop(S)
          end;
        error ->
          ServerPid ! {MsgRef, notInTheGame},
          loop(S)
      end;

    {ServerPid, MsgRef, {isBluff, BlufferPlayerPid}} ->
      case orddict:find(BlufferPlayerPid, S#state.players) of
        {ok, BlufferPlayer} ->
          flow ! {isBluff, lists:nth(1, BlufferPlayer)},


          CurrentPile = S#state.pile,
          flow ! {enter, CurrentPile},
          UpdatedPile = lists:append(CurrentPile, lists:nth(2, BlufferPlayer)),
          NewPlayers = orddict:erase(BlufferPlayerPid, S#state.players),
          UpdatedPlayers = orddict:store(BlufferPlayerPid, [lists:nth(1, BlufferPlayer), UpdatedPile], NewPlayers),

          loop(S#state{players = UpdatedPlayers, pile = []});
        error ->
          ServerPid ! {MsgRef, notInTheGame},
          loop(S)
      end;

    {ServerPid, MsgRef, {isNotBluff, DeclaringPlayerPid, BlufferPlayerPid}} ->
      case orddict:find(DeclaringPlayerPid, S#state.players) of
        {ok, DeclaringPlayer} ->
          flow ! {isNotBluff, lists:nth(1, DeclaringPlayer)},


          CurrentPile = S#state.pile,
          UpdatedPile = lists:append(CurrentPile, lists:nth(2, DeclaringPlayer)),
          NewPlayers = orddict:erase(DeclaringPlayerPid, S#state.players),
          UpdatedPlayers = orddict:store(DeclaringPlayerPid, [lists:nth(1, DeclaringPlayer), UpdatedPile], NewPlayers),

          case orddict:find(BlufferPlayerPid, S#state.players) of
            {ok, Player} ->
              Cards = lists:nth(2, Player),
              case length(Cards) == 0 of
                true -> flow ! {win, lists:nth(1, Player)};
                false -> loop(S#state{players = UpdatedPlayers, pile = []})
              end;
            error -> loop(S#state{players = UpdatedPlayers, pile = []})
          end;
        error ->
          ServerPid ! {MsgRef, notInTheGame},
          loop(S)
      end;

    {ServerPid, MsgRef, {cards, PlayerPid}} ->
      case orddict:find(PlayerPid, S#state.players) of
        {ok, Player} ->
          Cards = lists:nth(2, Player),
          ServerPid ! {MsgRef, cards, Cards},
          loop(S);
        error ->
          ServerPid ! {MsgRef, notInTheGame},
          loop(S)
      end;

    {ServerPid, MsgRef, {play, PlayerPid, Cards}} ->
      %% check if player is in the game
      case orddict:find(PlayerPid, S#state.players) of
        {ok, Player} ->
          CurrentTurn = lists:nth(1, lists:nth(1, S#state.turns)),
          %% check if is player's turn
          case PlayerPid == CurrentTurn of
            true ->
              CurrentDeck = lists:nth(2, Player),
              case lists:nth(1, Cards) =< length(CurrentDeck) of
                true ->
                  {CardsPlayed, OldCards} = lists:split(lists:nth(1, Cards), CurrentDeck),
                  NewPile = case length(S#state.pile) /= 0 of
                              true -> lists:append(CardsPlayed, S#state.pile);
                              false -> CardsPlayed
                            end,
                  NewPlayers = orddict:erase(PlayerPid, S#state.players),
                  UpdatedPlayers = orddict:store(PlayerPid, [lists:nth(1, Player), OldCards], NewPlayers),

                  %% call nextTurn to rotate array
                  NextTurn = nextTurn(S, CardsPlayed, lists:nth(2, Cards)),
                  NextPlayerPid = lists:nth(1, lists:nth(1, S#state.turns)),

                  case orddict:find(NextPlayerPid, S#state.players) of
                    {ok, Next} -> flow ! {play, lists:nth(1, Player), Cards, lists:nth(1, Next)},
                      ServerPid ! {MsgRef, endTurn, OldCards}
                  end,
                  loop(S#state{players = UpdatedPlayers, pile = NewPile, turns = NextTurn});

                false ->
                  ServerPid ! {MsgRef, error_3},
                  loop(S)
              end;

            false -> ServerPid ! {MsgRef, error_1},
              loop(S)
          end;
        %% if NO throw an error
        error ->
          ServerPid ! {MsgRef, error},
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
    {Ref, ok, Deck} -> PlayerPid ! {ok, Deck};
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
    {Ref, ok} -> PlayerPid ! ok;
    {Ref, alreadyReady} -> PlayerPid ! alreadyReady;
    {Ref, notInTheGame} -> PlayerPid ! notInTheGame;
    {Ref, deck} -> PlayerPid ! deck;
    {Ref, error, Reason} -> io:format("~p~n", [Reason])
  after 5000 ->
    {error, timeout}
  end.

play(PlayerPid, Cards) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {play, PlayerPid, Cards}},
  receive
    {Ref, ok} -> PlayerPid ! ok;
    {Ref, endTurn, NewDeck} -> PlayerPid ! {endTurn, NewDeck};
    {Ref, error} -> PlayerPid ! error;
    {Ref, error_1} -> PlayerPid ! error_1;
    {Ref, error_2} -> PlayerPid ! error_2;
    {Ref, error_3} -> PlayerPid ! error_3
  after 5000 ->
    {error, timeout}
  end.

bluff(PlayerPid) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {bluff, PlayerPid}},
  receive
    {Ref, isBluff, BlufferPlayerPid} ->
      ?MODULE ! {self(), Ref, {isBluff, BlufferPlayerPid}},
      PlayerPid ! isBluff;
    {Ref, isNotBluff, DeclaringPlayerPid, BlufferPlayerPid} ->
      ?MODULE ! {self(), Ref, {isNotBluff, DeclaringPlayerPid, BlufferPlayerPid}},
      PlayerPid ! isNotBluff;
    {Ref, notInTheGame} -> PlayerPid ! notInTheGame
  after 5000 ->
    {error, timeout}
  end.

cards(PlayerPid) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cards, PlayerPid}},
  receive
    {Ref, cards, Cards} -> PlayerPid ! {cards, Cards};
    {Ref, notInTheGame} -> PlayerPid ! notInTheGame
  after 5000 ->
    {error, timeout}
  end.


make_deck() ->
  [{Value, Suit} || Value <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
    Suit <- ["Clubs", "Diamonds", "Hearts", "Spades"]].

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

%% split lists into N parts
split([], _) -> [];
split(L, N) when length(L) < N -> [L];
split(L, N) ->
  {A, B} = lists:split(N, L),
  [A | split(B, N)].

ceiling(X) when X < 0 ->
  trunc(X);
ceiling(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.


nextTurn(S, CardsPlayed, Rank) ->
  case length(CardsPlayed) == 0 of
    true -> [[H, _], T] = S#state.turns,
      [T, [H, [[], ""]]];
    false -> [[H, _], T] = S#state.turns,
      [T, [H, [CardsPlayed, Rank]]]
  end.
