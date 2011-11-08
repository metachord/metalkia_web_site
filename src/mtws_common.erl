-module(mtws_common).

-export([
  set_user/1
]).

-export([
  blog_name/0,
  title/0,
  url/0,
  username/0,
  profile_link/0
]).

-include_lib("nitrogen_core/include/wf.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").

blog_name() ->
  "Metalkia blog".

title() -> "Metalkia".

url() ->
  ?DBG("URL", []),
  "http://metalkia.com".

username() ->
  User = wf:user(),
  if User =/= undefined ->
    User;
    true ->
      FbName = wf:session(facebook_name),
      if FbName =/= undefined ->
        mt_facebook:username_text();
        true ->
          TwName = wf:session(twitter_name),
          if TwName =/= undefined ->
              mt_twitter:username_text();
            true ->
              "undefined"
          end
      end
  end.


profile_link() ->
  User = wf:user(),
  if User =/= undefined ->
      "#";
    true ->
      FbLink = wf:session(facebook_link),
      if FbLink =/= undefined ->
        mt_facebook:profile_link();
        true ->
          TwName = wf:session(twitter_name),
          if TwName =/= undefined ->
              mt_twitter:profile_link();
            true ->
              "#"
          end
      end
  end.

set_user(User) ->
  wf:user(User).
