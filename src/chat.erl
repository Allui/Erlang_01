% написать приложение - чат
% пользователи хранятся в списке
%основной процесс сервера:
%loop(Users)->..

%подключение пользователя к серверу
%connect(User, Server) ->

%отключение
%disconnect(User,Server) -> ..

%отправить сообщение
%send(message,User,Server) ->

%Сервер просто печатает сообщение
%Сервер оповещвет о подключении/отключении пользователей

%Дома:
%Для каждого клиента должен быть самостоятельный процесс
% процесс сервера необходимо зарегистрировать как chat_server;

-module(chat).
-export([connect/2,disconnect/2,send/3, loop/1, start/0]).

start()->
    spawn(chat,loop,[[]]).

loop(Users)-> 
    receive
          {connect, User} ->
               io:format("~p~n", [{User, connected}]),
               loop([User|Users]);
          {disconnect, User} -> 
              io:format("~p~n", [{User, disconnected}]),
              loop([X || X <- Users, X =/= User]);
          {send, User, Message} -> 
              io:format("~p~n", [{User, Message}]), 
              loop(Users)
    end.
    
 connect(User, Server) -> 
     Server ! {connect, User}.
     
 disconnect(User, Server) ->
     Server ! {disconnect, User}.
     
 send(Message,User,Server) ->
     Server ! {send,User,Message}.
