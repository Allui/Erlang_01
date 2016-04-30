-module(ttt).
-export([start_server/0,
         new_gamer/1,
         find_game/1,
         create_game/1,
         server_loop/1,
         gamer_loop/1,
         room_loop/3,
         pid_exists/2,
         turn/2,
         leave_game/1]).

%% ~constructors
start_server()->
    io:fwrite("Server started~n"),
    global:register_name(game_server, spawn(ttt,server_loop,[[]])).

new_gamer(Nickname)->
    io:format("~p~n", [{hello, Nickname}]),
    spawn(ttt,gamer_loop,[Nickname]).

%%Gamer methods
create_game(Gamer) ->
    Gamer ! {create_game}.

find_game(Gamer) ->
    Gamer ! {find_game}.

turn(Gamer, Position) ->
    Gamer ! {turn, Position}.

leave_game(Gamer) ->
    Gamer ! {leave}.

%%Game loop
server_loop(Rooms)->
    receive
     {create_game, GPid, Nickname} ->
       BOOL = pid_exists(GPid,Rooms),
        if
          BOOL ->
                send(GPid,"~p~n",[{error, gamer_in_the_room}]),
                server_loop(Rooms);
          true ->
                send(GPid,"~p~n",[{room_created}]),
                server_loop([{spawn(ttt,room_loop,[create_board(),GPid,[{GPid, Nickname, ".x."}]]), [GPid]}|Rooms])
        end;

     {find_game, GPid, Nickname} ->
       BOOL = pid_exists(GPid,Rooms),
            if
                BOOL ->
                    send(GPid,"~p~n", [{error, gamer_in_the_room}]),
                    server_loop(Rooms);
                true ->
                    find_game(GPid, Nickname, Rooms),
                    server_loop(Rooms)
             end;

     {close_game,CRPid} ->
         server_loop([{RPid,Pids}||{RPid,Pids} <- Rooms, RPid =/= CRPid]);

     {change_pids, RPid, New_pids} ->
         server_loop([{RPid, New_pids}|[{Pid,GPids}||{Pid,GPids} <- Rooms, Pid =/= RPid]]);

     {leave, GPid, Nickname} ->
         find_game_and_execute_command(GPid, Rooms,{leave, GPid, Nickname}),
          server_loop(Rooms);

     {turn, GPid, Position} ->
         find_game_and_execute_command(GPid, Rooms,{turn, GPid, Position}),
         server_loop(Rooms)

    end.

    %%Room loop
    room_loop(Pole,CurrentPid,Gamers)->
        Pid = self(),
        receive
         {connect, GPid, Nickname} ->
              room_loop(Pole,CurrentPid,room_connect_execute(Pole,Gamers,GPid, Nickname));
         {leave, GPid, Nickname} ->
              room_leave_execute(Pid,Gamers, GPid, Nickname);
         {turn, GPid, Position} ->
                if
                    GPid == CurrentPid ->
                         send(GPid,"~p~n", [{error, not_your_turn}]),
                         room_loop(Pole,CurrentPid,Gamers);
                    true ->
                          [Value] = [V||{P,_,V} <- Gamers, P==GPid],
                          [Nickname] = [N||{P,N,_} <- Gamers, P=/=GPid],
                          [{P1,N1,V1},{P2,N2,V2}] = Gamers,
                          Board = turn_execute(GPid,Pole,Position,Value),
                          draw([P1,P2], Board, keys()),
                          broadcast([P1,P2],"turn of ~p~n", [Nickname]),
                          V1BOOL = check_win(V1,Board),
                          V2BOOL = check_win(V2,Board),
                          V3BOOL = no_turns(Board),
                          if
                             V1BOOL -> broadcast([P1,P2],"~p won! Room closed!~n", [N1]),
                                       global:send(game_server,  {close_game, Pid});
                             V2BOOL -> broadcast([P1,P2],"~p won! Room closed!~n", [N2]),
                                       global:send(game_server, {close_game, Pid});
                             V3BOOL -> broadcast([P1,P2],"Dead heat! Room closed!~n", []),
                                       global:send(game_server, {close_game, Pid});
                             true ->
                               if
                                 Board == Pole ->
                                     room_loop(Pole,CurrentPid,Gamers);
                                 true ->
                                     room_loop(Board,GPid,Gamers)
                               end
                          end
                end
    end.

room_connect_execute(Pole,Gamers,GPid, Nickname) ->
  Gs = [{GPid, Nickname, ".o." }|Gamers],
  Pids = [P||{P,_,_} <- Gs],
  draw(Pids,Pole,keys()),
  broadcast(Pids,"turn of ~p~n", [Nickname]),
  Gs.

room_leave_execute(Pid, Gamers, GPid, Nickname) ->
  [Nick] = [N||{P,N,_} <- Gamers, P=/=GPid],
  Pids = [P||{P,_,_} <- Gamers],
  broadcast(Pids,"~p won, because ~p left the game!~n Room closed!~n", [Nick,Nickname]),
  global:send(game_server, {close_game, Pid}).

%%Gamer loop
gamer_loop(Nickname)->
    Pid = self(),
    receive
     {create_game} ->
             global:send(game_server, {create_game, Pid, Nickname}),
             gamer_loop(Nickname);
     {find_game} ->
             global:send(game_server, {find_game, Pid, Nickname}),
             gamer_loop(Nickname);
     {turn, Position} ->
             global:send(game_server, {turn, Pid, Position}),
             gamer_loop(Nickname);
     {leave} ->
             global:send(game_server, {leave, Pid, Nickname}),
             gamer_loop(Nickname);
     {console,String, List} ->
             io:format(String,List),
             gamer_loop(Nickname)
    end.

pid_exists(_,[]) -> false;
pid_exists(Pid,[{_,[Pid]}|_])-> true;
pid_exists(Pid,[{_,[Pid,_]}|_])-> true;
pid_exists(Pid,[{_,[_,Pid]}|_])-> true;
pid_exists(Pid,[_|T])-> pid_exists(Pid,T).

find_game(GPid, _, [])->
     send(GPid,"~p~n", [{error, free_room_not_found}]);
find_game(GPid, Nickname, [{RPid, Pids}|T]) ->
      case Pids of
        [H|[]] -> global:send(game_server, {change_pids, RPid, [H,GPid]}),
                   RPid ! {connect, GPid, Nickname};
        [_|_] -> find_game(GPid,Nickname,T)
      end.

find_game_and_execute_command(GPid,[],_) ->
    send(GPid,"~p~n", [{error, running_game_not_found}]);
find_game_and_execute_command(GPid,[H|T], Command) ->
      case H of
        {RPid,[GPid,_]} -> RPid ! Command;
        {RPid,[_,GPid]} -> RPid ! Command;
        {_,_} -> find_game_and_execute_command(GPid,T, Command)
      end.

%%Support
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

  send(Pid, String, List) ->
     Pid !  {console, String, List}.
