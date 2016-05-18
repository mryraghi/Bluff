%%%-------------------------------------------------------------------
%%% @author Romeo
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2016 18:57
%%%-------------------------------------------------------------------
-module(game).
-author("Romeo").

%% API
-export([start/0, init/0, enter/2, terminate/0, ready/0, leave/1]).

%% the server will need to keep two things in its state:
%%    a list of subscribing players
%%    a list of all the event processes it spawned

-record(state, {players}).

start() ->
  %% the atom ?MODULE can be used instead of the Pid
  register(?MODULE, Pid = spawn(?MODULE, init, [])),

  %% make the atom 'listen' available everywhere in this document
  register(ready, spawn(?MODULE, ready, [])),

  %% run 'init'
  Pid,

  io:format("Type 'game:ready().' to enter game's flow!~n").

%%% GAME'S FLOW
ready() ->
  receive
    {enter, PlayerName, No_Players} ->
      io:format("-->  ~p has entered the game! (~p players [Min: 2, Max: 10]).~n", [PlayerName, No_Players]),
      ready();
    {leave, PlayerName, No_Players} ->
      io:format("-->  ~p has left the game! (~p players [Min: 2, Max: 10]).~n", [PlayerName, No_Players]),
      ready()
  end.

terminate() ->
  ?MODULE ! shutdown.

init() ->
  loop(#state{players = orddict:new()}).

loop(S = #state{}) ->
  receive
  %% monitor the client
    {ServerPid, CtoS_Ref, {enter, PlayerName, ClientPid}} ->
      case orddict:size(S#state.players) + 1 =< 2 of
        true ->
          case orddict:find(ClientPid, S#state.players) of
            {ok, _E} ->
              ServerPid ! {CtoS_Ref, error_1},
              S#state.players,
              loop(S);
            error ->
              StoC_Ref = erlang:monitor(process, ClientPid),
              NewPlayers = orddict:store(ClientPid, [StoC_Ref, PlayerName], S#state.players),
              ServerPid ! {CtoS_Ref, ok},
              ready ! {enter, PlayerName, orddict:size(NewPlayers)},
              loop(S#state{players = NewPlayers})
          end;
        false -> ServerPid ! {CtoS_Ref, error_2}
      end;

  %% for debugging purposes
    {ServerPid, MsgRef, {leave, ClientPid}} ->
      case orddict:find(ClientPid, S#state.players) of
        {ok, Player} ->
          NewPlayers = orddict:erase(ClientPid, S#state.players),
          ServerPid ! {MsgRef, ok},
          ready ! {leave, lists:nth(2, Player), orddict:size(NewPlayers)},
          loop(S#state{players = NewPlayers});
        error ->
          ServerPid ! {MsgRef, error, "You've already exited the game!~nType 'game:enter(self()).' to join."},
          loop(S)
      end;
    shutdown ->
      exit(shutdown);
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.

enter(PlayerName, ClientPid) ->
  %% monitor the server
  CtoS_Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), CtoS_Ref, {enter, PlayerName, ClientPid}},
  receive
    {CtoS_Ref, ok} -> io:format("You've been accepted to play!~n");
    {CtoS_Ref, error_1} -> io:format("You've alerady entered the game!~n");
    {CtoS_Ref, error_2} -> io:format("Maximum number of players reached!~n");
  %% if the server unexpectedly shuts down receive an error
    {'DOWN', CtoS_Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.


leave(ClientPid) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {leave, ClientPid}},
  receive
    {Ref, ok} -> ok;
    {Ref, error, Reason} -> io:format("~p~n", [Reason])
  after 5000 ->
    {error, timeout}
  end.