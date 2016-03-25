-module(min).
-export([reduce/3,filter/2]).

reduce(_, Start, []) -> Start;
reduce(F, Start,[H|T]) -> reduce(F,F(H,Start),T).
    
filter(_,[])->[];
filter(P,L) -> [X || X <- L, P(X)].