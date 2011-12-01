-module(mtws_common).

-export([
  set_email/1,
  get_email/0,
  set_state/1,
  get_state/0,
  update_external_profile/1
]).

-export([
  blog_name/0,
  blog_link/0,
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
  PathInfo = wf_context:path_info(),
  case dict:find(blog_title, PathInfo) of
    {ok, BlogTitle} ->
      BlogTitle;
    error ->
      "Metalkia"
  end.

blog_link() ->
  wf:session_default(blog_link, mtc:get_env(url)).

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
  User = wf:user(),
  UserName = user_or_name(),
  SignString =
  case UserName of
    "" -> "Login";
    UN -> UN
  end,
  [
    case UserName of
      "" ->
        CurrentUrl = dict:fetch(current_url, wf:path_info()),
        CurrentDomain = dict:fetch(current_domain, wf:path_info()),
        ?DBG("CurrentUrl: ~p", [CurrentUrl]),
        ["<iframe width=\"0\" height=\"0\" src=\"" ++
          mtc:get_env(url) ++ "/sso" ++
          "?action=auth" ++
          "&return_url=" ++ mtc_util:uri_encode(CurrentUrl) ++
          "&auth_domain=" ++ mtc_util:uri_encode(CurrentDomain) ++
          "\"></iframe>"];
      _ -> []
    end,
    #list{class = "topnav", body = [
      #listitem{body = #link{url = "#", text = SignString}},
      #listitem{body = #list{class = "subnav", body = [
        #listitem{body = mt_facebook:login_panel()},
        #listitem{body = mt_twitter:login_panel()},
        if
          User =/= undefined ->
            #listitem{body = mt_logoff:logoff_panel()};
          true ->
            []
        end
      ]}}
    ]}
  ].

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

-record(session_state, {
  user,
  email,
  facebook_id,
  facebook_name,
  twitter_id,
  twitter_name
}).

get_state() ->
  #session_state{
    user = wf:user(),
    email = wf:session(user_email),
    facebook_id = wf:session(facebook_id),
    facebook_name = wf:session(facebook_name),
    twitter_id = wf:session(twitter_id),
    twitter_name = wf:session(twitter_name)
  }.

set_state(undefined) ->
  ok;
set_state(#session_state{
             user = User,
             email = Email,
             facebook_id = FacebookId,
             facebook_name = FacebookName,
             twitter_id = TwitterId,
             twitter_name = TwitterName
            }) ->
  wf:user(User),
  wf:session(user_email, Email),
  wf:session(facebook_id, FacebookId),
  wf:session(facebook_name, FacebookName),
  wf:session(twitter_id, TwitterId),
  wf:session(twitter_name, TwitterName).
