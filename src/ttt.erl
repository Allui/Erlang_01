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
    register(game_server, spawn(ttt,server_loop,[[]])).

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
                io:format("~p~n", [{error, gamer_in_the_room}]),
                server_loop(Rooms);
          true ->
                io:format("~p~n", [{ok, room_created}]),
                server_loop([{spawn(ttt,room_loop,[create_board(),GPid,[{GPid, Nickname, ".x."}]]), [GPid]}|Rooms])
        end;

     {find_game, GPid, Nickname} ->
       BOOL = pid_exists(GPid,Rooms),
            if
                BOOL ->
                    io:format("~p~n", [{error, gamer_in_the_room}]),
                    server_loop(Rooms);
                true ->
                    find_game(GPid, Nickname, Rooms),
                    io:format("~p~n", [{ok}]),
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
              draw(Pole,keys()),
              room_loop(Pole,CurrentPid,[{GPid, Nickname, ".o." }|Gamers]);
         {leave, GPid, Nickname} ->
              [Nick] = [N||{P,N,_} <- Gamers, P=/=GPid],
              io:format("~p won, because ~p left the game!~n Room closed!~n", [Nick,Nickname]),
              game_server ! {close_game, Pid};
         {turn, GPid, Position} ->
                if
                    GPid == CurrentPid ->
                         io:format("~p~n", [{error, not_your_turn}]),
                         room_loop(Pole,CurrentPid,Gamers);
                    true ->
                          [Value] = [V||{P,_,V} <- Gamers, P==GPid],
                          Board = turn_execute(Pole,Position,Value),
                          draw(Board,keys()),
                          [{_,N1,V1},{_,N2,V2}] = Gamers,
                          V1BOOL = check_win(V1,Board),
                          V2BOOL = check_win(V2,Board),
                          if
                             V1BOOL -> io:format("~p won! Room closed!~n", [N1]),
                                       game_server ! {close_game, Pid};
                             V2BOOL -> io:format("~p won! Room closed!~n", [N2]),
                                       game_server ! {close_game, Pid};
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

%%Gamer loop
gamer_loop(Nickname)->
    Pid = self(),
    receive
     {create_game} ->
             game_server ! {create_game, Pid, Nickname},
             gamer_loop(Nickname);
     {find_game} ->
             game_server ! {find_game, Pid, Nickname},
             gamer_loop(Nickname);
     {turn, Position} ->
             game_server ! {turn, Pid, Position},
             gamer_loop(Nickname);
      {leave} ->
             game_server ! {leave, Pid, Nickname},
             gamer_loop(Nickname)
    end.

pid_exists(_,[]) -> false;
pid_exists(Pid,[{_,[Pid]}|_])-> true;
pid_exists(Pid,[{_,[Pid,_]}|_])-> true;
pid_exists(Pid,[{_,[_,Pid]}|_])-> true;
pid_exists(Pid,[_|T])-> pid_exists(Pid,T).

find_game(_, _, [])->
     io:format("~p~n", [{error, free_room_not_found}]);
find_game(GPid, Nickname, [{RPid, Pids}|T]) ->
      case Pids of
        [H|[]] -> game_server ! {change_pids, RPid, [H,GPid]},
                   RPid ! {connect, GPid, Nickname};
        [_|_] -> find_game(GPid,Nickname,T)
      end.

find_game_and_execute_command(_,[],_) ->
     io:format("~p~n", [{error, running_game_not_found}]);
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

turn_execute(Board, Position, Value) ->
  BOOL = can_pass_value(Board, Position),
  if
    BOOL ->
       [{Position,Value}|[{K,V}||{K,V} <- Board, K=/=Position]];
    true ->
       io:format("~p~n", [{error, not_correct_turn}]),
       Board
  end.

can_pass_value([], _) -> false;
can_pass_value([{K,". ."}|_],K) -> true;
can_pass_value([_|T],K) -> can_pass_value(T,K).

find_par(_,[]) -> false;
find_par({K,V},[{K,V}|_]) -> true;
find_par(Par,[_|T])-> find_par(Par,T).

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

draw(Board, [H|T]) ->
  if
    H == s ->
        io:fwrite("Board:~n"),
        draw(Board,T);
    H == r ->
        io:fwrite("~n"),
        draw(Board,T);
    H == e ->
        io:fwrite("~n");
    true ->
        io:format("~p",[get(H,Board)]),
        draw(Board,T)
  end.
