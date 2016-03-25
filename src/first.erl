-module(first).
-export([hi/0, fact/1, fib/1, member/2, cost/1]).

hi() ->
	io:format("Hello!").

fact(0) -> 1;
fact(N) -> N*fact(N-1).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1)+fib(N-2).

member([],_) -> false;
member([H|_],H) -> true;
member([_|T],E) -> member(T,E).	

price(apple) -> 100;
price(pear) -> 150;
price(milk) -> 70;
price(beef) -> 400;
price(pork) -> 300.

%[{apple, count}, ...]
%cost(L)-> get summ

cost([]) -> 0;
cost([{S,C}|T]) -> price(S)*C+cost(T).