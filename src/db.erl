-module(db).
-export([new/0, put/3, delete/2,find/2,get/2]).


%db:new()
%db:put(K,V,DB)
%db:delete(K,DB)
%db:get(K,DB)
%db:find(V,DB)

%Db = db:new().
%Db1 = db:put(k1,v1,Db).

new()->[].

put(K,V,[]) -> [{K,V}];
put(K,V,Db) -> [{K,V}|delete(K,Db)].

delete(K,Db) -> [{K1,V1}||{K1,V1} <- Db, K=/=K1].

get(K,Db) -> getValue([{K1,V1}||{K1,V1} <- Db, K==K1]).

getValue([]) -> {error, not_exists};
getValue([{_,V}|_])->V.

find(V,Db) -> findeResult([{K1,V1}||{K1,V1} <- Db, V==V1]).
findeResult([]) -> {error, not_finde};
findeResult(L) -> [K1||{K1,_} <- L].
