-module(crawler_downloder).
-behaviour(gen_server).

-export([start/0, download/2]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2, code_change/3]).

  %%----------------------------------------------------------------------------
  start() ->
    ID = get_id(),
    gen_server:start_link({global, ID}, ?MODULE, [], []),
    ID.

download(ID, Img) ->
    gen_server:cast({global, ID}, {download, Img}).



  %%----------------------------------------------------------------------------
  init([])->
      inets:start(),
      {ok, []}.
  terminate(_Reason, _State) -> ok.
  handle_info(_Message, State) -> { noreply, State }.
  code_change(_OldVersion, State, _Extra) -> { ok, State }.
  %%----------------------------------------------------------------------------

handle_call(_Message, _Form, State) -> {reply, ok, State}.

handle_cast({download, Img}, State) ->
  try
     request(Img),
    {noreply, State}
  catch
    _:_ ->
    {noreply, State}
  end;

handle_cast(_Message, State) -> { noreply, State }.

%%--Support---------------------------------------------------------------------
%rexp() -> ["^https?://.*", "^/.*/$"].

request(Img) ->
    {R, {{_Type, Code, _Ok}, _Headers, Body}} =  httpc:request(Img),
    if
      (R == ok) and (Code == 200) ->
        file:write_file("/Images"++name(Img), Body)
    end.

name(URL) ->
    {ok, {http,_,_Root,_Port,Path,_Query}} = http_uri:parse(URL),
    string:sub_string(Path,string:rstr(Path,"/"), string:len(Path)).

get_id() ->
        list_to_atom(integer_to_list(erlang:system_time())).
