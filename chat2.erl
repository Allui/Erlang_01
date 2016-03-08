-module(chat2).
-export([connect/1,disconnect/1,send/2, loop/1, start/0, new_user/1, client_loop/1]).

%Создание сервера
start()->
    register(chat_server, spawn(chat2,loop,[[]])).

%Новый пользователь
new_user(User_name)->
    spawn(chat2,client_loop,[User_name]).

%Петля сервера
loop(Pids)-> 
    receive
          {connect, Pid, User_name} ->
               io:format("~p~n", [{User_name, connected}]),
               loop([Pid|Pids]);
          {disconnect, Pid, User_name} -> 
               io:format("~p~n", [{User_name, disconnected}]),
               loop([X || X <- Pids, X =/= Pid]);
          {send, Pid, User_name, Message} ->
               io:format("~p~n", [{message_from, User_name}]),
                broadcast(Message, [X || X <- Pids, X =/= Pid]), 
                loop(Pids)
    end.

%рассылка сообщений
broadcast(_,[])->
     io:format("~p~n", [{ok, completed}]);
broadcast(Message,[H|T]) ->
    H ! {listen,Message},
    broadcast(Message, T).
    
 %подключение 
 connect(User) -> 
     User ! {connect}.
 
 %отключение 
 disconnect(User) ->
     User ! {disconnect}.
 
 %отправка сообщения
 send(User,Message) ->
     User ! {send,Message}.
 
 %петля клиента  
 client_loop(User_name)->
     Pid = self(),
     receive
         {connect} -> 
             chat_server ! {connect, Pid, User_name},
             client_loop(User_name);
         {disconnect} ->
              chat_server ! {disconnect, Pid, User_name};
         {send, Message} ->
             chat_server ! {send,  Pid, User_name, Message},
             client_loop(User_name);
         {listen, Message} ->
              io:format("~p~n", [{User_name,Message, heared}]),
             client_loop(User_name)
     end.
