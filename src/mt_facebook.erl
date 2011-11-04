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
  Action = wf:q(action),
  if Action =:= "logoff" ->
      SignedRequest = wf:q(signed_request),
      ?DBG("LogOff: signed_request=~p", [SignedRequest]),
      ok;
     true ->
      Code = wf:q(code),
      Req =
        "https://graph.facebook.com/oauth/access_token?"
        "client_id=" ++ app_id() ++ "&"
        "redirect_uri=" ++ mtc_util:uri_encode(login_redirect_uri()) ++ "&"
        "client_secret=" ++ app_secret() ++ "&"
        "code="++Code
        ,
      %% TODO: rewrite with FSM process
      case httpc:request(Req) of
        {ok,{{"HTTP/1.1",200,"OK"}, Headers, Body}} ->
          ?DBG("Facebook reply:~n~p~n~p", [Headers, Body]),
          Params = [list_to_tuple(re:split(PV, "=", [{return, list}])) || PV <- re:split(Body, "&", [{return, list}])],
          AccessToken = proplists:get_value("access_token", Params, ""),

          MeReq =
            "https://graph.facebook.com/me?"
            "access_token=" ++ AccessToken ++ "&"
            "client_id=" ++ app_id() ++ "&"
            "redirect_uri=" ++ mtc_util:uri_encode(login_redirect_uri()) ++ "&"
            "code="++Code
            ,
          case httpc:request(MeReq) of
            {ok,{{"HTTP/1.1",200,"OK"}, MeHeaders, MeBody}} ->
              ?DBG("Facebook Me reply:~n~p~n~p", [MeHeaders, mochijson2:decode(MeBody)]),
              %% TODO: Get user info, store ID in database
              case mochijson2:decode(MeBody) of
                {struct, MeFields} ->
                  FbProfile =
                    lists:foldl(
                      fun({<<"id">>, Val}, Fb) -> Fb#mt_facebook{id = Val};
                         ({<<"name">>, Val}, Fb) -> Fb#mt_facebook{name = Val};
                         ({<<"first_name">>, Val}, Fb) -> Fb#mt_facebook{first_name = Val};
                         ({<<"middle_name">>, Val}, Fb) -> Fb#mt_facebook{middle_name = Val};
                         ({<<"last_name">>, Val}, Fb) -> Fb#mt_facebook{last_name = Val};
                         ({<<"link">>, Val}, Fb) -> Fb#mt_facebook{link = Val};
                         ({<<"gender">>, Val}, Fb) -> Fb#mt_facebook{gender = ?a2gender(Val)};
                         ({<<"email">>, Val}, Fb) -> Fb#mt_facebook{email = Val};
                         ({<<"timezone">>, Val}, Fb) -> Fb#mt_facebook{timezone = ?a2i(Val)};
                         ({<<"locale">>, Val}, Fb) -> Fb#mt_facebook{locale = Val};
                         ({<<"updated_time">>, Val}, Fb) -> Fb#mt_facebook{updated_time = Val}
                      end,
                      #mt_facebook{}, MeFields),

                  FriendsReq =
                    "https://graph.facebook.com/me/friends?"
                    "access_token="++AccessToken
                    ,
                  Friends =
                    case httpc:request(FriendsReq) of
                      {ok,{{"HTTP/1.1",200,"OK"}, FriendsHeaders, FriendsBody}} ->
                        ?DBG("Facebook Friends reply:~n~p~n~p", [FriendsHeaders, mochijson2:decode(FriendsBody)]),
                        %% TODO: Search friends IDs in database, prompt to add on metalkia
                        case mochijson2:decode(FriendsBody) of
                          {struct, FriendsData} ->
                            FriendsList = proplists:get_value(<<"data">>, FriendsData, []),
                            [#mt_fb_friend{id = proplists:get_value(<<"id">>, F),
                                           name = proplists:get_value(<<"name">>, F)} ||
                              {struct, F} <- FriendsList];
                          FJsonError ->
                            ?ERR("Cannot decode friends list:~n~p", [FJsonError]),
                            []
                        end;
                      FriendsError ->
                        ?ERR("Facebook friends request error:~n~p", [FriendsError]),
                        []
                    end,
                  %% Store new Facebook profile
                  mtc_entry:sput(FbProfile#mt_facebook{friends = Friends});
                MJsonError ->
                  ?ERR("Cannot decode me reply:~n~p", [MJsonError]),
                  []
              end;
            MeError ->
              ?ERR("Facebook Me request error:~n~p", [MeError]),
              error
          end;
        Error ->
          ?ERR("Facebook request error:~n~p", [Error]),
          error
      end
  end,
  wf:redirect(mtc:get_env(url, "http://metalkia.com")).
