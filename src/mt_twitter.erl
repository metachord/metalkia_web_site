-module(mt_twitter).

-export([
  main/0,
  login_panel/0
]).

%% For template
-export([
  is_signed_in/0,
  profile_link/0,
  username_text/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include_lib("oauth/include/oauth.hrl").

main() ->
  Consumer = #oauth_consumer{
    key = key(),
    secret = secret(),
    method = hmac_sha1
   },
  TwitterName = wf:session(twitter_name),
  RequestTokenSecretSess = wf:session(twitter_oauth_token_secret),
  if
    TwitterName =:= undefined ->
      if
        RequestTokenSecretSess =:= undefined ->
          RequestTokenUrl = request_token_url(),
          {ok, RequestTokenResponse} = oauth:get(RequestTokenUrl, [], Consumer),
          RequestTokenParams = oauth:params_decode(RequestTokenResponse),
          ?DBG("RequestTokenParams: ~p", [RequestTokenParams]),
          RequestToken = oauth:token(RequestTokenParams),
          wf:session(twitter_oauth_token, RequestToken),
          RequestTokenSecret = oauth:token_secret(RequestTokenParams),
          wf:session(twitter_oauth_token_secret, RequestTokenSecret),
          wf:redirect(authorize_url() ++ "?oauth_token=" ++ RequestToken);
        true ->
          AccessTokenURL = access_token_url(),
          RequestTokenSess = wf:session(twitter_oauth_token),
          {ok, AccessTokenResponse} = oauth:get(AccessTokenURL, [], Consumer, RequestTokenSess, RequestTokenSecretSess),
          AccessTokenParams = oauth:params_decode(AccessTokenResponse),
          ?DBG("AccessTokenParams: ~p", [AccessTokenParams]),
          ScreenName = proplists:get_value("screen_name", AccessTokenParams),
          wf:session(twitter_name, ScreenName),

          %% TODO: move following code to async process
          LookupUrl = "https://api.twitter.com/1/users/lookup.json?include_entities=true&screen_name=" ++ ScreenName,
          case httpc:request(LookupUrl) of
            {ok,{{"HTTP/1.1",200,"OK"}, LookupHeaders, LookupBody}} ->
              ?DBG("Twitter lookup reply:~n~p~n~p", [LookupHeaders, LookupBody]),
              case mochijson2:decode(LookupBody) of
                [{struct, LookupFields}] ->
                  TwProfile =
                    lists:foldl(
                      fun({<<"id_str">>, Val}, Tw) -> Tw#mt_twitter{id = Val};
                         ({<<"screen_name">>, Val}, Tw) -> Tw#mt_twitter{screen_name = Val};
                         ({<<"url">>, Val}, Tw) -> Tw#mt_twitter{url = Val};
                         ({<<"name">>, Val}, Tw) -> Tw#mt_twitter{name = Val};
                         ({<<"time_zone">>, Val}, Tw) -> Tw#mt_twitter{timezone = Val};
                         ({<<"utc_offset">>, Val}, Tw) -> Tw#mt_twitter{utc_offset = ?a2i(Val)};
                         ({<<"description">>, Val}, Tw) -> Tw#mt_twitter{description = Val};
                         ({<<"lang">>, Val}, Tw) -> Tw#mt_twitter{locale = Val};
                         (_ParVal, Tw) -> Tw
                      end,
                      #mt_twitter{}, LookupFields),
                  FriendsUrl = "https://api.twitter.com/1/friends/ids.json?cursor=-1&stringify_ids=true&screen_name=" ++ ScreenName,
                  Friends =
                    case httpc:request(FriendsUrl) of
                      {ok,{{"HTTP/1.1",200,"OK"}, FriendsHeaders, FriendsBody}} ->
                        ?DBG("Twitter friends ids reply:~n~p~n~p", [FriendsHeaders, FriendsBody]),
                        case mochijson2:decode(FriendsBody) of
                          {struct, FriendsData} ->
                            FriendsIds = proplists:get_value(<<"ids">>, FriendsData),
                            [#mt_tw_friend{id = Id} || Id <- FriendsIds];
                          FJsonError ->
                            ?ERR("Cannot decode friends list:~n~p", [FJsonError]),
                            []
                        end;
                      FriendsError ->
                        ?ERR("Twitter friends request error:~n~p", [FriendsError]),
                        []
                    end,
                  case mtc_entry:supdate(TwProfile#mt_twitter{friends = Friends}) of
                    {updated, #mt_twitter{metalkia_id = MetalkiaId} = _SProfile} when MetalkiaId =/= undefined ->
                      %% This user already has Metalkia profile
                      %% #mt_person{username = MetalkiaUser} = mtc_entry:sget(mt_person, MetalkiaId),
                      wf:session(metalkia_id, binary_to_list(MetalkiaId)),
                      wf:user(binary_to_list(MetalkiaId)),
                      wf:redirect(mtc:get_env(url));
                    _ ->
                      %% This user has not profile
                      wf:redirect(mtc:get_env(url) ++ "/profile")
                  end;
                LJsonError ->
                  ?ERR("Cannot decode lookup reply:~n~p", [LJsonError]),
                  []
              end;
            LookupError ->
              ?ERR("Twitter lookup request error:~n~p", [LookupError]),
              error
          end
      end;
    true ->
      wf:redirect(mtc:get_env(url))
  end.

login_panel() ->
  #panel{body = #template{file = "./site/templates/metalkia/twitter_service.html"}}.

%%

is_signed_in() ->
  ScreenName = wf:session(twitter_name),
  if
    ScreenName =:= undefined ->
      false;
    true ->
      true
  end.

profile_link() ->
  ScreenName = wf:session(twitter_name),
  if
    ScreenName =:= undefined ->
      mtc:get_env(url) ++ "/twitter";
    true ->
      "http://twitter.com/" ++ ScreenName
  end.

username_text() ->
  ScreenName = wf:session(twitter_name),
  if
    ScreenName =:= undefined ->
      "Sign in with Twitter";
    true ->
      "@" ++ ScreenName
  end.

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
