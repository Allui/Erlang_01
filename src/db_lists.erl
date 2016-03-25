-module(db_lists).
-export([new/0, put/3, delete/2,find/2,get/2]).


%db:new()
%db:put(K,V,DB)
%db:delete(K,DB)
%db:get(K,DB)
%db:find(V,DB)

%Db = db:new().
%Db1 = db:put(k1,v1,Db).

new()->[].

put(K,V,Db) ->
    lists:keystore(K, 1, Db, {K, V}).

delete(K,Db) ->
    lists:keydelete(K, 1, Db).

get(K,Db) -> getValue(lists:keysearch(K, 1, Db)).

getValue({_,{_,V}})->V;
getValue(_) -> {error, not_exists}.

find(V,Db) -> 
    findeResult(
        lists:filter(fun({_,Value})-> Value == V end,Db)
        ).
findeResult([]) -> {error, not_finde};
findeResult(L) -> [K1||{K1,_} <- L].
