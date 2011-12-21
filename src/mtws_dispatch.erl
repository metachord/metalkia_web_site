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
    {["sso"],                ?MODULE, mt_sso},
    %% Static content handlers
    {["css", '*'], static_resource, [{root, "./site/static/metalkia/css"}]},
    {["images", '*'], static_resource, [{root, "./site/static/metalkia/images"}]},
    {["nitrogen", '*'], static_resource, [{root, "./site/static/nitrogen"}]},
    {["mt", '*'], static_resource, [{root, "./site/static/metalkia"}]}
  ] ++
  [{Path, ?MODULE, []} || Path <- [
    []
    , ["profile"]
    , ["logoff"]
    , ["facebook"]
    , ["twitter"]
    , ["post"]
    , ["post", post_id]
    , ["post-add"]
    , ["blog", blog_id]
    , ["blog", blog_id, "post", post_id]
    , ["blog", blog_id, "post-add"]
    , ["info"]
  ]] ++
  [
    {['*'], static_resource, [{root, "./site/static/metalkia"}]}
  ].

init(PageModule) ->
  State = #state { page_module=PageModule },
  {ok, State}.

allowed_methods(ReqData, State) ->
  {['HEAD', 'GET', 'POST'], ReqData, State}.

content_types_provided(Request, Context) ->
  {[{"text/html", to_html}],
    Request, Context}.

is_protected(_Module) ->
  true.

realm() ->
  "Metalkia".

is_authenticated(_Module, _User) ->
  false.

authenticate(_Module, _User, _Password) ->
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
  RequestBridge = simple_bridge:make_request(webmachine_request_bridge, Req),
  ResponseBridge = simple_bridge:make_response(webmachine_response_bridge, Req),
  nitrogen:init_request(RequestBridge, ResponseBridge),

  PathInfo = wrq:path_info(Req),
  PathInfo1 = dict:store(host_tokens, wrq:host_tokens(Req), PathInfo),
  CurrentBaseUri = atom_to_list(wrq:scheme(Req)) ++ "://" ++ string:join(lists:reverse(wrq:host_tokens(Req)), "."),
  CurrentUrl = CurrentBaseUri ++ wrq:raw_path(Req),
  PathInfo2 = dict:store(current_base_uri, CurrentBaseUri, PathInfo1),
  PathInfo3 = dict:store(current_url, CurrentUrl, PathInfo2),
  wf_context:path_info(PathInfo3),

  [nitrogen:handler(Handler, PageModule) ||
  Handler <- [
      mt_route_handler,
      mt_session_handler,
      mt_security_handler,
      mt_identity_handler
  ]],
  nitrogen:run().
