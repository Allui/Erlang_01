-module(crawler).
-behaviour(gen_server).

-export([start/0, run/1]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2, code_change/3]).

  %%----------------------------------------------------------------------------
  start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []),
    crawler_store:start().

  run(URL) ->
    gen_server:call({global, ?MODULE}, {run, URL}).
  %%----------------------------------------------------------------------------
  init([])->
      inets:start(),
      {ok, []}.
  terminate(_Reason, _State) -> ok.
  handle_info(_Message, State) -> { noreply, State }.
  code_change(_OldVersion, State, _Extra) -> { ok, State }.
  %%----------------------------------------------------------------------------

handle_call({run, URL}, _From, State) ->
            crawler_store:store(URL),
            Cid = crawler_worker:start(),
            crawler_worker:get_map(Cid, URL),
            {reply, ok, State};

handle_call(_Message, _Form, State) -> {reply, ok, State}.
handle_cast(_Message, State) -> { noreply, State }.

%%--Support---------------------------------------------------------------------
%rexp() -> ["^https?://.*", "^/.*/$"].
