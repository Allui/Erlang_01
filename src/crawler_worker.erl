-module(crawler_worker).
-behaviour(gen_server).

-export([start/0, get_map/2]).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2, code_change/3]).

  %%----------------------------------------------------------------------------
  start() ->
    ID = get_id(),
    gen_server:start_link({global, ID}, ?MODULE, [], []),
    ID.

  get_map(ID, URL) ->
    gen_server:cast({global, ID}, {get_map, URL}).



  %%----------------------------------------------------------------------------
  init([])->
      inets:start(),
      {ok, []}.
  terminate(_Reason, _State) -> ok.
  handle_info(_Message, State) -> { noreply, State }.
  code_change(_OldVersion, State, _Extra) -> { ok, State }.
  %%----------------------------------------------------------------------------

handle_call(_Message, _Form, State) -> {reply, ok, State}.

handle_cast({get_map, URL}, State) ->
  try
    {R, {{_Type, Code, _Ok}, _Headers, Body}} =  httpc:request(URL),
    if
      (R == ok) and (Code == 200) ->
          tree_worker(URL,Body),
          {noreply, State};
      true ->
          {noreply, State}
    end
  catch
    _:_ ->
    {noreply, State}
  end;

handle_cast(_Message, State) -> { noreply, State }.

%%--Support---------------------------------------------------------------------
%rexp() -> ["^https?://.*", "^/.*/$"].

tree_worker(URL, Body) ->
        Tree = mochiweb_html:parse(Body),
        Hrefs = remove_duplicates(mochiweb_xpath:execute("//a/@href",Tree)),
        Imgs = remove_duplicates(mochiweb_xpath:execute("//img/@src",Tree)),
        lists:foreach(fun(Img) -> download_image(url(URL), Img) end, Imgs),
        lists:foreach(fun(Href) -> spawn_workers(url(URL), Href) end, Hrefs).

spawn_workers(URL, Href) ->
  case re:run(Href,"^/.*",[{capture,none}]) of
      match ->
        FullURL = bjoin([list_to_binary(URL), Href]),
        case need_stop(FullURL) of
          false ->
              crawler_store:store(FullURL),
              Cid = crawler_worker:start(),
              crawler_worker:get_map(Cid, binary_to_list(FullURL));
          true ->
              nomatch
          end;
      nomatch ->
        nomatch
  end.

download_image(URL, Image) ->
  case re:run(Image,"^(http:\\/\\/|\\.\\.).*",[{capture,none}]) of
      match ->
          nothing;
      nomatch ->
        FullURL = bjoin([list_to_binary(URL), Image]),
        ID = crawler_downloder:start(),
        crawler_downloder:download(ID, binary_to_list(FullURL))
  end.

need_stop(URL) ->
  [C, L] = crawler_store:get_base(),
  (C > 5000) or
  lists:member(URL,L).

  url(URL) ->
      {ok, {http,_,Root,_Port,Path,_Query}} = http_uri:parse(URL),
      Ctx = string:sub_string(Path,1, string:rstr(Path,"/")),
      "http://"++Root++Ctx.


remove_duplicates(L) ->
        sets:to_list(sets:from_list(L)).

bjoin(List) ->
        F = fun(A, B) -> <<A/binary, B/binary>> end,
        lists:foldr(F, <<>>, List).

get_id() ->
        list_to_atom(integer_to_list(erlang:system_time())).
