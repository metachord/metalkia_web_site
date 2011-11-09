-module(mtws_dispatch).

-export([
  init/1,
  is_protected/1,
  realm/0,
  authenticate/3,
  is_authenticated/2,
  content_types_provided/2,
  to_html/2,
  allowed_methods/2,
  post_is_create/2,
  process_post/2
]).

-export([
  rules/0
]).


-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").

-record(state, {page_module}).

rules() ->
  [
    %% Dynamic content handlers
    {[],                     ?MODULE, mt_index},
    {["post", post_id],      ?MODULE, mt_post},
    {["logoff"],             ?MODULE, mt_logoff},
    {["facebook"],           ?MODULE, mt_facebook},
    {["twitter"],            ?MODULE, mt_twitter},
    {["profile"],            ?MODULE, mt_profile},
    %% Static content handlers
    {["css", '*'], static_resource, [{root, "./site/static/metalkia/css"}]},
    {["images", '*'], static_resource, [{root, "./site/static/metalkia/images"}]},
    {["nitrogen", '*'], static_resource, [{root, "./site/static/nitrogen"}]},
    {["mt", '*'], static_resource, [{root, "./site/static/metalkia"}]},

    {['*'], ?MODULE, []}
    %%{['*'], static_resource, [{root, "./site/static/metalkia"}]}
  ].

init(PageModule) ->
  ?DBG("PageModule: ~p", [PageModule]),
  State = #state { page_module=PageModule },
  {ok, State}.

allowed_methods(ReqData, State) ->
  {['HEAD', 'GET', 'POST'], ReqData, State}.

content_types_provided(Request, Context) ->
  {[{"text/html", to_html}],
    Request, Context}.

is_protected(_Module) ->
  ?DBG("Module: ~p", [_Module]),
  true.

realm() ->
  "Metalkia".

is_authenticated(_Module, _User) ->
  ?DBG("Module: ~p, ~p", [_Module, _User]),
  false.

authenticate(_Module, _User, _Password) ->
  ?DBG("Module: ~p, ~p:~p", [_Module, _User, _Password]),
  false.

to_html(ReqData, State) ->
  PageModule = State#state.page_module,
  {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
  {Data, ReqData1, State}.

post_is_create(ReqData, State) ->
  {false, ReqData, State}.

process_post(ReqData, State) ->
  PageModule = State#state.page_module,
  {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
  ReqData2 = wrq:set_resp_body(Data, ReqData1),
  {true, ReqData2, State}.

do_nitrogen(PageModule, Req) ->
  ?DBG("do_nitrogen(~p)", [PageModule]),
  RequestBridge = simple_bridge:make_request(webmachine_request_bridge, Req),
  ResponseBridge = simple_bridge:make_response(webmachine_response_bridge, Req),
  nitrogen:init_request(RequestBridge, ResponseBridge),

  PathInfo = wrq:path_info(Req),
  ?DBG("PathInfo: ~p", [PathInfo]),
  wf_context:path_info(PathInfo),

  [nitrogen:handler(Handler, PageModule) ||
  Handler <- [
      mt_route_handler,
      mt_session_handler,
      mt_security_handler,
      mt_identity_handler
  ]],

  PostParams = RequestBridge:post_params(),
  ?DBG("POST Params:~n~p", [PostParams]),

  GetParams = RequestBridge:query_params(),
  ?DBG("GET Params:~n~p", [GetParams]),

  EventModule = wf_context:event_module(),
  ?DBG("EventModule: ~p", [EventModule]),

  nitrogen:run().
