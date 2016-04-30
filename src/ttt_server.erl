-module(ttt_server).
-behaviour(gen_server).

-export([start_link/0, create_game/2, find_game/2, leave/2, turn/2, close_game/1]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2, code_change/3]).

  %%----------------------------------------------------------------------------
  start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

    create_game(GamerID,Name) ->
     gen_server:call({global, ?MODULE}, {create_game, GamerID, Name}).

close_game(RoomID) ->
   gen_server:call({global, ?MODULE}, {close_game, RoomID}).

     find_game(GamerID,Name) ->
       gen_server:call({global, ?MODULE}, {find_game, GamerID, Name}).

    leave(GamerID,Name) ->
         gen_server:call({global, ?MODULE}, {leave, GamerID, Name}).

    turn(GamerID, Position) ->
      gen_server:call({global, ?MODULE},  {turn, GamerID, Position}).

 change_pids(RPid, New_pids) ->
      gen_server:cast({global, ?MODULE},  {change_pids, RPid, New_pids}).


  %%----------------------------------------------------------------------------
  init([])->
    {ok, []}.
  terminate(_Reason, _State) -> ok.
  handle_info(_Message, State) -> { noreply, State }.
  code_change(_OldVersion, State, _Extra) -> { ok, State }.
  %%----------------------------------------------------------------------------

handle_call({create_game, GPid, Nickname}, _Form, Rooms) ->
      BOOL = pid_exists(GPid,Rooms),
       if
         BOOL ->
               send(GPid,"~p~n",[{error, gamer_in_the_room}]),
               {reply, ok, Rooms};
         true ->
               send(GPid,"~p~n",[{room_created}]),
               {reply, ok, [{ttt_room:start(GPid, Nickname), [GPid]}|Rooms]}
       end;

handle_call({find_game, GPid, Nickname}, _Form, Rooms) ->
  BOOL = pid_exists(GPid,Rooms),
       if
           BOOL ->
               send(GPid,"~p~n", [{error, gamer_in_the_room}]),
               {reply, ok, Rooms};
           true ->
               find_game(GPid, Nickname, Rooms),
               {reply, ok, Rooms}
        end;

handle_call({close_game, RoomID}, _Form, Rooms) ->
      {reply, ok,[{RPid,Pids}||{RPid,Pids} <- Rooms, RPid =/= RoomID]};


handle_call({leave, GPid, Nickname}, _Form, Rooms) ->
               find_game_and_execute_command(GPid, Rooms,{leave, GPid, Nickname}),
               {reply, ok, Rooms};

handle_call({turn, GPid, Position}, _Form, Rooms) ->
                    find_game_and_execute_command(GPid, Rooms,{turn, GPid, Position}),
                    {reply, ok, Rooms}.


handle_cast({change_pids, RPid, New_pids}, Rooms) ->
           {noreply,[{RPid, New_pids}|[{Pid,GPids}||{Pid,GPids} <- Rooms, Pid =/= RPid]]};
handle_cast(_Message, State) -> { noreply, State }.

%%--Support---------------------------------------------------------------------
find_game(GPid, _, [])->
     send(GPid,"~p~n", [{error, free_room_not_found}]);
find_game(GPid, Nickname, [{RPid, Pids}|T]) ->
      case Pids of
        [H|[]] ->  change_pids(RPid, [H,GPid]),
                   ttt_room:connect(RPid, GPid, Nickname);
        [_|_] -> find_game(GPid,Nickname,T)
      end.

find_game_and_execute_command(GPid,[],_) ->
          send(GPid,"~p~n", [{error, running_game_not_found}]);
find_game_and_execute_command(GPid,[H|T], {Atom, GPid, Data}) ->
            case H of
              {RPid,[GPid,_]} -> execute(Atom,RPid, GPid, Data);
              {RPid,[_,GPid]} -> execute(Atom,RPid, GPid, Data);
              {_,_} -> find_game_and_execute_command(GPid,T, {Atom, GPid, Data})
            end.

execute(Atom, RPid, ID, Data) ->
    case Atom of
       turn -> ttt_room:turn(RPid, ID, Data);
       leave -> ttt_room:leave(RPid, ID, Data)
    end.

pid_exists(_,[]) -> false;
pid_exists(Pid,[{_,[Pid]}|_])-> true;
pid_exists(Pid,[{_,[Pid,_]}|_])-> true;
pid_exists(Pid,[{_,[_,Pid]}|_])-> true;
pid_exists(Pid,[_|T])-> pid_exists(Pid,T).

send(GamerID, String, List) ->
   ttt_gamer:console(GamerID,String, List).
