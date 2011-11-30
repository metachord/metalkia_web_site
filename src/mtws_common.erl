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
  user_or_name/0,
  name/0,
  profile_link/0,
  copyright/0,
  sign/0
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

user_or_name() ->
  User = wf:user(),
  if User =/= undefined ->
      User;
     true ->
      name()
  end.

profile_link() ->
  User = wf:user(),
  if User =/= undefined ->
      "/profile";
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

sign() ->
  IsTwSig = mt_twitter:is_signed_in(),
  IsFbSig = mt_facebook:is_signed_in(),
  SignString =
  case user_or_name() of
    "" -> "Login";
    UN -> UN
  end,
  #list{class = "topnav", body = [
    #listitem{body = #link{url = "#", text = SignString}},
    #listitem{body = #list{class = "subnav", body = [
      #listitem{body = mt_facebook:login_panel()},
      #listitem{body = mt_twitter:login_panel()},
      if
        IsTwSig orelse IsFbSig ->
          #listitem{body = mt_logoff:logoff_panel()};
        true ->
          []
      end
    ]}}
  ]}.

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
      NewProfile =
        lists:foldl(
          fun({facebook_id, Id}, Prof) ->
              if Id =:= undefined -> Prof;
                 true -> Prof#mt_person{facebook_id = ?a2b(Id)}
              end;
             ({twitter_id, Id}, Prof) ->
              if Id =:= undefined -> Prof;
                 true -> Prof#mt_person{twitter_id = ?a2b(Id)}
              end
          end,
          Profile,
          [{facebook_id, FbId}, {twitter_id, TwId}]),
      mtc_entry:sput(NewProfile);
    _ ->
      ?DBG("Facebook profile not found: ~p", [FbId])
  end.

copyright() ->
  PathInfo = wf:path_info(),
  case dict:find(blog, PathInfo) of
    {ok, _BlogName} ->
      %% Fetch Copyright of blog
      "";
    error ->
      "© Metalkia, 2011"
  end.
