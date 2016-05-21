-module(crawler_store).
-behaviour(gen_server).

-export([start/0, store/1, print/0, save/0, get_base/0]).
-export([init/1,
         handle_call/3,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

  %%----------------------------------------------------------------------------
  start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

  store(URL) ->
    gen_server:cast({global, ?MODULE}, {store, URL}).

  print() ->
    gen_server:cast({global, ?MODULE}, {print}).

  save() ->
      gen_server:cast({global, ?MODULE}, {save}).

  get_base() ->
     gen_server:call({global, ?MODULE}, {get_base}).


  %%----------------------------------------------------------------------------
  init([])->
      inets:start(),
      {ok, [0, []]}.
  terminate(_Reason, _State) -> ok.
  handle_info(_Message, State) -> { noreply, State }.
  code_change(_OldVersion, State, _Extra) -> { ok, State }.
  %%----------------------------------------------------------------------------

handle_call({get_base}, _Form, State) ->
  {reply, State, State};
handle_call(_Message, _Form, State) -> {reply, ok, State}.

handle_cast({store, URL}, [Count, Array]) ->
    io:format("~p~n",[Count]),
    { noreply, [Count + 1,[URL|Array]] };
handle_cast({print}, [Count, Array]) ->
    io:format("~p~n",[Array]),
    { noreply,  [Count, Array] };
handle_cast({save},  [Count, Array]) ->
        save(remove_duplicates(Array)),
        { noreply,  [Count, Array] };
handle_cast(_Message, State) -> { noreply, State }.

%%--Support---------------------------------------------------------------------
save(Data) ->
  file:write_file("/test/map.txt",io_lib:fwrite("~p.\n",[Data])).

remove_duplicates(L) ->
          sets:to_list(sets:from_list(L)).

 %file:write_file("/test/map.txt",io_lib:fwrite("~p.\n",[Data])).
