-module(min_lists).
-export([reduce/3,filter/2]).

reduce(_, Start, []) -> Start;
reduce(F, Start,[H|T]) -> reduce(F,F(H,Start),T).
    
filter(P,L) -> lists:filter(P,L).