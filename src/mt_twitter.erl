-module(mt_twitter).

-export([
  main/0
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
  if TwitterName =:= undefined ->
      RequestTokenSecretSess = wf:session(twitter_oauth_token_secret),
      if RequestTokenSecretSess =:= undefined ->
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
          wf:session(twitter_name, proplists:get_value("screen_name", AccessTokenParams))
      end;
     true ->
      ok
  end,
  wf:redirect(mtc:get_env(url)).
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
