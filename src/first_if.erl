-module(first_if).
-export([hi/0, fact/1, fib/1, member/2, cost/1]).

hi() ->
	io:format("Hello!").

fact(N) -> 
    if 
        N == 0 -> 1;
        true -> N*fact(N-1)
    end.

fib(N) -> 
    if 
        N == 0 -> 0;
        N == 1 -> 1;
        true -> fib(N-1)+fib(N-2)
    end.

member(S,E) ->
    case S of 
        [] -> false;
        [H|_] when H == E -> true;
        [_|T] -> member(T,E)
    end.

price(Item) -> 
    case Item of 
        apple -> 100;
        pear -> 150;
        milk -> 70;
        beef -> 400;
        pork -> 300
    end.

%[{apple, count}, ...]
%cost(L)-> get summ
cost(L) ->
    case L of 
        [] -> 0;
        [{S,C}|T] -> price(S)*C+cost(T)
    end.