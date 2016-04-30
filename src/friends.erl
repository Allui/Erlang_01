- module(friends).

- export([user/3, find_link/2, start/0, get_user/2]).

- record(user, {name="",age,friends=[user("",0,[])]}).

user(Name, Age, Friends)->
  #user{name = Name, age = Age, friends = Friends}.

start()->
  L =  [user("Edward", 18, []),
        user("Alphons", 17, []),
        user("Stephany", 20, []),
        user("Arnold", 13, []),
        user("Antony", 14, []),
        user("Akali", 15, []),
        user("Gregory", 11, []),
        user("Nataly", 24, []),
        user("Henk", 35, [])],

 L1 = lists:map(fun(E) -> add_friend(E,get_random(L)) end,L),
 lists:map(fun(E1) -> add_friend(E1,get_random(L1)) end,L1).


get_random(List) ->
  Index = random:uniform(length(List)),
  lists:nth(Index,List).

get_user(Index, List) ->
  lists:nth(Index,List).

add_friend(User,Friend)->
  if
    (User#user.name =/= Friend#user.name) and (User#user.age =/= Friend#user.age) ->
      User#user{friends = [Friend|User#user.friends]};
    true ->
      User
  end.

find_link(User1,User2)->
  find(User1,User2#user.friends,1).

find(_User,[],_Acc) -> io:write("-");
find(User, [H|T], Acc)->
  if
    (User#user.name == H#user.name) and (User#user.age == H#user.age) ->
       io:format("~p~n",[{link_finded, Acc}]);
    true ->
       find(User,H#user.friends, Acc+1), find(User,T, Acc)
  end.
