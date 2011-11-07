-module(mt_twitter).

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
  main/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include_lib("oauth/include/oauth.hrl").

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

get_config() ->
  mtc:get_env(twitter, []).

key() ->
  proplists:get_value(key, get_config()).

secret() ->
  proplists:get_value(secret, get_config()).

%% callback_url() ->
%%   proplists:get_value(callback_url, get_config()).

request_token_url() ->
  proplists:get_value(request_token_url, get_config()).

authorize_url() ->
  proplists:get_value(authorize_url, get_config()).

access_token_url() ->
  proplists:get_value(access_token_url, get_config()).

main() ->
  Consumer = #oauth_consumer{
    key = key(),
    secret = secret(),
    method = hmac_sha1
   },
  RequestTokenSecretSess = wf:session(oauth_token_secret),
  if RequestTokenSecretSess =:= undefined ->
      RequestTokenUrl = request_token_url(),
      {ok, RequestTokenResponse} = oauth:get(RequestTokenUrl, [], Consumer),
      RequestTokenParams = oauth:params_decode(RequestTokenResponse),
      ?DBG("RequestTokenParams: ~p", [RequestTokenParams]),
      RequestToken = oauth:token(RequestTokenParams),
      wf:session(oauth_token, RequestToken),
      RequestTokenSecret = oauth:token_secret(RequestTokenParams),
      wf:session(oauth_token_secret, RequestTokenSecret),
      wf:redirect(authorize_url() ++ "?oauth_token=" ++ RequestToken);
     true ->
      AccessTokenURL = access_token_url(),
      RequestTokenSess = wf:session(oauth_token),
      {ok, AccessTokenResponse} = oauth:get(AccessTokenURL, [], Consumer, RequestTokenSess, RequestTokenSecretSess),
      AccessTokenParams = oauth:params_decode(AccessTokenResponse),
      ?DBG("AccessTokenParams: ~p", [AccessTokenParams]),
      wf:session(twitter_name, proplists:get_value("screen_name", AccessTokenParams)),
      wf:redirect(mtc:get_env(url))
  end.
