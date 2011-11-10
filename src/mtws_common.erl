-module(mtws_common).

-export([
  set_email/1,
  get_email/0,
  update_external_profile/1
]).

-export([
  blog_name/0,
  blog_link/0,
  title/0,
  url/0,
  name/0,
  profile_link/0
]).

-include_lib("nitrogen_core/include/wf.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").
-include_lib("metalkia_core/include/mt_records.hrl").

blog_name() ->
  "Metalkia blog".

blog_link() ->
  wf:session_default(blog_link, mtc:get_env(url)).

title() -> "Metalkia".

url() ->
  ?DBG("URL", []),
  mtc:get_env(url).

name() ->
  FbName = wf:session(facebook_name),
  if FbName =/= undefined ->
    mt_facebook:username_text();
    true ->
      TwName = wf:session(twitter_name),
      if TwName =/= undefined ->
        mt_twitter:username_text();
        true ->
          ""
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

set_email(Email) ->
  wf:session(user_email, Email).

get_email() ->
  wf:session(user_email).

update_external_profile(MetalkiaId) ->
  FbId = wf:session(facebook_id),
  TwId = wf:session(twitter_id),

  case mtc_entry:sget(mt_facebook, ?a2b(FbId)) of
    #mt_facebook{} = FbProfile ->
      mtc_entry:sput(FbProfile#mt_facebook{metalkia_id = ?a2b(MetalkiaId)});
    _ ->
      ?DBG("Facebook profile not found: ~p", [FbId])
  end,
  case mtc_entry:sget(mt_twitter, ?a2b(TwId)) of
    #mt_twitter{} = TwProfile ->
      mtc_entry:sput(TwProfile#mt_twitter{metalkia_id = ?a2b(MetalkiaId)});
    _ ->
      ?DBG("Twitter profile not found: ~p", [TwId])
  end,

  case mtc_entry:sget(mt_person, ?a2b(MetalkiaId)) of
    #mt_person{} = Profile ->
      NewProfile = Profile#mt_person{
        facebook_id = if FbId =/= undefined -> ?a2b(FbId); true -> undefined end,
        twitter_id = if TwId =/= undefined -> ?a2b(TwId); true -> undefined end
      },
      mtc_entry:sput(NewProfile);
    _ ->
      ?DBG("Facebook profile not found: ~p", [FbId])
  end.


