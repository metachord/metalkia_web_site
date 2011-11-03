-module(mt_facebook).

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
  app_id/0,
  data_perms/0,
  login_redirect_uri/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-record(state, {page_module}).

init([]) ->
  PageModule = ?MODULE,
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
  RequestBridge = simple_bridge:make_request(webmachine_request_bridge, Req),
  ResponseBridge = simple_bridge:make_response(webmachine_response_bridge, Req),
  nitrogen:init_request(RequestBridge, ResponseBridge),
  nitrogen:handler(mt_session_handler, PageModule),
  nitrogen:handler(mt_security_handler, PageModule),
  nitrogen:handler(mt_route_handler, PageModule),
  nitrogen:handler(mt_identity_handler, PageModule),

  Params = RequestBridge:post_params(),
  ?DBG("POST Params:~n~p", [Params]),

  PathInfo = wrq:path_info(Req),
  wf_context:path_info(PathInfo),
  nitrogen:run().

%%

app_id() ->
  "255033837865445".

data_perms() ->
  "email,user_checkins".

login_redirect_uri() ->
  "http://metalkia.com/facebook".
