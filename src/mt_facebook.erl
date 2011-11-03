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
  login_redirect_uri/0,
  main/0
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

  [nitrogen:handler(Handler, PageModule) ||
  Handler <- [
      mt_session_handler,
      mt_security_handler,
      mt_route_handler,
      mt_identity_handler
  ]],

  PostParams = RequestBridge:post_params(),
  ?DBG("POST Params:~n~p", [PostParams]),

  GetParams = RequestBridge:query_params(),
  ?DBG("GET Params:~n~p", [GetParams]),

  PathInfo = wrq:path_info(Req),
  wf_context:path_info(PathInfo),
  nitrogen:run().

%%

app_id() ->
  mtc:get_env(facebook_app_id, "dummy_app_id").

app_secret() ->
  mtc:get_env(facebook_app_secret, "dummy_app_secret").

data_perms() ->
  "email".

login_redirect_uri() ->
  mtc:get_env(url, "http://metalkia.com") ++ "/facebook".

main() ->
  Code = wf:q(code),
  Req =
    "https://graph.facebook.com/oauth/access_token?"
    "client_id=" ++ app_id() ++ "&"
    "redirect_uri=" ++ mtc_util:uri_encode(login_redirect_uri()) ++ "&"
    "client_secret=" ++ app_secret() ++ "&"
    "code="++Code
    ,
  case httpc:request(Req) of
    {ok,{{"HTTP/1.1",200,"OK"}, Headers, Body}} ->
      ?DBG("Facebook reply:~n~p~n~p", [Headers, Body]),
      Params = [list_to_tuple(re:split(PV, "=", [{return, list}])) || PV <- re:split(Body, "&", [{return, list}])],
      MeReq =
        "https://graph.facebook.com/me?"
        "access_token=" ++ proplists:get_value("access_token", Params, "") ++ "&"
        "client_id=" ++ app_id() ++ "&"
        "redirect_uri=" ++ mtc_util:uri_encode(login_redirect_uri()) ++ "&"
        "code="++Code
        ,
      case httpc:request(MeReq) of
        {ok,{{"HTTP/1.1",200,"OK"}, MeHeaders, MeBody}} ->
          ?DBG("Facebook Me reply:~n~p~n~p", [MeHeaders, MeBody]),
          ok;
        MeError ->
          ?ERR("Facebook Me error:~n~p", [MeError]),
          error
      end;
    Error ->
      ?ERR("Facebook error:~n~p", [Error]),
      error
  end,

  wf:redirect(mtc:get_env(url, "http://metalkia.com")).
