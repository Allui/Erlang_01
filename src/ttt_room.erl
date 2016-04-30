-module(ttt_room).

-behaviour(gen_server).

-export([start/2, connect/3, leave/3, turn/3, stop/1]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2, code_change/3]).

%%----------------------------------------------------------
start(GamerID,Name) ->
  RoomID = get_id(),
  gen_server:start_link({global, RoomID}, ?MODULE, [RoomID, create_board(),GamerID,[{GamerID, Name, ".x."}]], []),
  RoomID.

connect(RoomID, GamerID,Name) ->
 gen_server:cast({global, RoomID}, {connect, GamerID, Name}).

leave(RoomID, GamerID, Name) ->
  gen_server:cast({global, RoomID}, {leave, GamerID, Name}).

turn(RoomID, GamerID, Position) ->
  gen_server:cast({global, RoomID},  {turn, GamerID, Position}).

stop(RoomID)->
  gen_server:cast({global, RoomID},{stop}).

%%----------------------------------------------------------
init(Args)->
  {ok, Args}.

terminate(_Reason, _State) -> ok.
handle_info(_Message, State) -> { noreply, State }.
handle_call(_Message,_From,State) -> {reply, ok, State}.
code_change(_OldVersion, State, _Extra) -> { ok, State }.
%%----------------------------------------------------------


handle_cast({leave, GamerID, Name}, [RoomID,Pole,CurrentPid,Gamers]) ->
  room_leave_execute(RoomID, Gamers, GamerID, Name),
  {noreply, [RoomID,Pole,CurrentPid,Gamers]};

handle_cast({turn, GamerID, Position}, [RoomID,Pole,CurrentPid,Gamers]) ->
if
    GamerID == CurrentPid ->
         send(GamerID,"~p~n", [{error, not_your_turn}]),
         {noreply, [RoomID,Pole,CurrentPid,Gamers]};
    true ->
          [Value] = [V||{P,_,V} <- Gamers, P==GamerID],
          [Nickname] = [N||{P,N,_} <- Gamers, P=/=GamerID],
          [{P1,N1,V1},{P2,N2,V2}] = Gamers,
          Board = turn_execute(GamerID,Pole,Position,Value),
          draw([P1,P2], Board, keys()),
          broadcast([P1,P2],"turn of ~p~n", [Nickname]),
          V1BOOL = check_win(V1,Board),
          V2BOOL = check_win(V2,Board),
          V3BOOL = no_turns(Board),
          if
             V1BOOL -> broadcast([P1,P2],"~p won! Room closed!~n", [N1]),
                       ttt_room:stop(RoomID),
                        {noreply, [RoomID,Pole,CurrentPid,Gamers]};
             V2BOOL -> broadcast([P1,P2],"~p won! Room closed!~n", [N2]),
                       ttt_room:stop(RoomID),
                        {noreply, [RoomID,Pole,CurrentPid,Gamers]};
             V3BOOL -> broadcast([P1,P2],"Dead heat! Room closed!~n", []),
                       ttt_room:stop(RoomID),
                        {noreply, [RoomID,Pole,CurrentPid,Gamers]};
             true ->
               if
                 Board == Pole ->
                     {noreply, [RoomID,Pole,CurrentPid,Gamers]};
                 true ->
                     {noreply, [RoomID,Board,GamerID,Gamers]}
               end
          end
end;


handle_cast({connect, GamerID, Name}, [RoomID,Pole,CurrentPid,Gamers]) ->
  {noreply, [RoomID,Pole,CurrentPid,room_connect_execute(Pole,Gamers,GamerID, Name)]};

handle_cast({stop}, [RoomID,Pole,CurrentPid,Gamers]) ->
          ttt_server:close_game(RoomID),
          {stop, normal, [RoomID,Pole,CurrentPid,Gamers]};

handle_cast(_Message, State) -> { noreply, State }.


%%----------------------------------------------------------
%%Support

room_connect_execute(Pole,Gamers,GPid, Nickname) ->
              Gs = [{GPid, Nickname, ".o." }|Gamers],
              Pids = [P||{P,_,_} <- Gs],
              draw(Pids,Pole,keys()),
              broadcast(Pids,"turn of ~p~n", [Nickname]),
              Gs.

room_leave_execute(RoomID, Gamers, GPid, Nickname) ->
  [Nick] = [N||{P,N,_} <- Gamers, P=/=GPid],
  Pids = [P||{P,_,_} <- Gamers],
  broadcast(Pids,"~p won, because ~p left the game!~n Room closed!~n", [Nick,Nickname]),
  ttt_room:stop(RoomID).

create_board() ->
  [{11,". ."},{12,". ."},{13,". ."},{21,". ."},{22,". ."},{23,". ."},{31,". ."},{32,". ."},{33,". ."}].
keys() ->
  [s,11,12,13,r,21,22,23,r,31,32,33,e].
  get(K,Db) -> getValue([{K1,V1}||{K1,V1} <- Db, K==K1]).

  getValue([]) -> io:format("~p~n", [{error, not_exists}]);
  getValue([{_,V}|_])->V.

turn_execute(GPid, Board, Position, Value) ->
  BOOL = can_pass_value(Board, Position),
  if
    BOOL ->
       [{Position,Value}|[{K,V}||{K,V} <- Board, K=/=Position]];
    true ->
       send(GPid,"~p~n", [{error, not_correct_turn}]),
       Board
  end.

can_pass_value([], _) -> false;
can_pass_value([{K,". ."}|_],K) -> true;
can_pass_value([_|T],K) -> can_pass_value(T,K).

find_par(_,[]) -> false;
find_par({K,V},[{K,V}|_]) -> true;
find_par(Par,[_|T])-> find_par(Par,T).

no_turns(Board) ->
  [X||{_,X} <- Board, X == ". ."] == [].

check_win(V,Board)->
  BOOL1 = find_par({11,V},Board) and find_par({12,V},Board) and find_par({13,V},Board),
  BOOL2 = find_par({21,V},Board) and find_par({22,V},Board) and find_par({23,V},Board),
  BOOL3 = find_par({31,V},Board) and find_par({32,V},Board) and find_par({33,V},Board),
  BOOL4 = find_par({11,V},Board) and find_par({21,V},Board) and find_par({31,V},Board),
  BOOL5 = find_par({12,V},Board) and find_par({22,V},Board) and find_par({32,V},Board),
  BOOL6 = find_par({13,V},Board) and find_par({23,V},Board) and find_par({33,V},Board),
  BOOL7 = find_par({11,V},Board) and find_par({22,V},Board) and find_par({33,V},Board),
  BOOL8 = find_par({13,V},Board) and find_par({22,V},Board) and find_par({31,V},Board),
  BOOL1 or BOOL2 or BOOL3 or BOOL4 or BOOL5 or BOOL6 or BOOL7  or BOOL8.

draw(Pids,Board, [H|T]) ->
  if
    H == s ->
        broadcast(Pids,"Board:~n",[]),
        draw(Pids,Board,T);
    H == r ->
        broadcast(Pids,"~n",[]),
        draw(Pids,Board,T);
    H == e ->
        broadcast(Pids,"~n",[]);
    true ->
        broadcast(Pids,"~p",[get(H,Board)]),
        draw(Pids,Board,T)
  end.

  broadcast([],_,_) -> {ok};
  broadcast([H|T],String,List) ->
     send(H,String,List),
     broadcast(T,String, List).

  send(GamerID, String, List) ->
     ttt_gamer:console(GamerID,String, List).

     get_id() ->
            list_to_atom(integer_to_list(erlang:system_time())).
