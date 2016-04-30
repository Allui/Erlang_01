-module(ttt_gamer).
-behaviour(gen_server).

-export([create/1, create_game/1, find_game/1, leave/1, turn/2, console/3]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2, code_change/3]).

%%----------------------------------------------------------
create(Name) ->
  ID = get_id(),
  gen_server:start_link({global, ID}, ?MODULE, [Name,ID], []),
  ID.

create_game(ID) ->
   gen_server:call({global, ID}, {create_game}).

find_game(ID) ->
    gen_server:call({global, ID}, {find_game}).

leave(ID) ->
    gen_server:call({global, ID}, {leave}).

turn(ID, Position) ->
    gen_server:call({global, ID},  {turn, Position}).

console(ID,String, List) ->
    gen_server:cast({global, ID},  {console,String, List}).

init(Args)->
  io:format("starting..... ~n",[]),
  {ok, Args}.
terminate(_Reason, _State) -> ok.
handle_info(_Message, State) -> { noreply, State }.
code_change(_OldVersion, State, _Extra) -> { ok, State }.
%%----------------------------------------------------------

handle_call({create_game}, _From,  [Name,ID]) ->
          ttt_server:create_game(ID, Name),
          {reply, ok, [Name,ID]};

handle_call({find_game}, _From,  [Name,ID]) ->
                  ttt_server:find_game(ID, Name),
                  {reply, ok, [Name,ID]};

handle_call({turn, Position}, _From,  [Name,ID]) ->
                  ttt_server:turn(ID, Position),
                  {reply, ok, [Name,ID]};

handle_call({leave}, _From,  [Name,ID]) ->
                  ttt_server:leave(ID, Name),
                  {reply, ok, [Name,ID]}.

handle_cast({console,String, List},  State) ->
                          io:format(String,List),
                          {noreply, State};

handle_cast(_Message, State) -> { noreply, State }.

%%Support

get_id() ->
       list_to_atom(integer_to_list(erlang:system_time())).
