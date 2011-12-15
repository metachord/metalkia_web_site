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
  base_uri/0,
  user_or_name/0,
  user_blog/0,
  user_blog/1,
  user_blog/2,
  name/0,
  is_logged_in/0,
  profile_link/0,
  copyright/0,
  menu/0,
  %% Google analytics stuff
  google_analytics/0,
  ga_account/1,
  ga_host/1
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
      mtc:get_env(default_blog_name)
  end.

blog_link() ->
  case dict:find(current_base_uri, wf:path_info()) of
    {ok, Uri} -> Uri;
    error -> mtc:get_env(url)
  end.

user_blog() ->
  user_blog(wf:user()).

user_blog(UserName) ->
  user_blog(UserName, "").

user_blog(undefined, _) ->
  mtc:get_env(url);
user_blog(UserName, Path) ->
  {UScheme, UHost, _UPath, _UArgs, _UPart} = mochiweb_util:urlsplit(mtc:get_env(url)),
  mochiweb_util:urlunsplit({UScheme, ?a2l(UserName) ++ "." ++ UHost, [Path], "", ""}).


url() ->
  case dict:find(current_url, wf:path_info()) of
    {ok, CurrentUrl} ->
      CurrentUrl;
    error ->
      mtc:get_env(url)
  end.

%% TODO: use same one from webmachine when seth/base_uri will be merged in upstream
base_uri() ->
  case dict:find(current_base_uri, wf:path_info()) of
    {ok, CurrentDomain} ->
      CurrentDomain;
    error ->
      mtc:get_env(url)
  end.

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

is_logged_in() ->
  User = wf:user(),
  FbId = wf:session(facebook_id),
  TwId = wf:session(twitter_id),
  (User =/= undefined)
  orelse (FbId =/= undefined)
  orelse (TwId =/= undefined)
  .

menu() ->
  PathInfo = wf:path_info(),
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
        CurrentBaseUri = dict:fetch(current_base_uri, wf:path_info()),
        ["<iframe width=\"0\" height=\"0\" src=\"" ++
          mtc:get_env(url) ++ "/sso" ++
          "?action=auth" ++
          "&return_url=" ++ mtc_util:uri_encode(CurrentUrl) ++
          "&auth_domain=" ++ mtc_util:uri_encode(CurrentBaseUri) ++
          "\"></iframe>"];
      _ -> []
    end,
    #list{class = "topnav", body = [
      #listitem{body = [
        #link{url = "#", text = SignString},
        #list{class = "subnav", body = [
          if
            User =/= undefined ->
              #listitem{body = [#link{url = user_blog(User, "/profile"), text = "Profile"}]};
            true ->
              []
          end,
          #listitem{body = mt_facebook:login_panel()},
          #listitem{body = mt_twitter:login_panel()},
          if
            User =/= undefined ->
              #listitem{body = mt_logoff:logoff_panel()};
            true ->
              []
          end
      ]}]},
      if
        User =/= undefined ->
          #listitem{body = [

            %% #link{url = "#", text = "Post"},
            %% #list{class = "subnav", body = [
            %%   case dict:find(blog_id, PathInfo) of
            %%     {ok, BN} ->
            %%       #listitem{body = #link{url = "/blog-post-add/" ++ BN, text = "Add post"}};
            %%     _ ->
            %%       #listitem{body = #link{url = "/post-add", text = "Add post"}}
            %%   end
            %% ]}
            case dict:find(blog_id, PathInfo) of
              {ok, BN} ->
                #link{url = "/blog-post-add/" ++ BN, text = "Add post"};
              _ ->
                #link{url = "/post-add", text = "Add post"}
            end
          ]};
        true ->
          []
      end,
      #listitem{body = [
        #link{url = "#", text = "Help"},
        #list{class = "subnav", body = [
          #listitem{body = #link{url = mtc:get_env(url) ++ "/info", text = "Info"}}
        ]}
      ]}
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

google_analytics() ->
  PathInfo = wf:path_info(),
  Account = case dict:find(ga_account, PathInfo) of
    {ok, AVal} ->
      AVal;
    _ -> undefined
  end,
  Host = case dict:find(ga_host, PathInfo) of
    {ok, HVal} ->
      HVal;
    _ -> undefined
  end,
  if
    Account =/= undefined ->
      #template {file="./site/templates/metalkia/google-analytics.tpl", bindings = [{'Val', {Account, Host}}]};
    true ->
      ""
  end.

ga_account({Account, _}) ->
  Account.

ga_host({_, Host}) ->
  Host.

-record(session_state, {
  user,
  email,
  facebook_id,
  facebook_name,
  facebook_link,
  twitter_id,
  twitter_name,
  redirect_url
}).

get_state() ->
  #session_state{
    user = wf:user(),
    email = wf:session(user_email),
    facebook_id = wf:session(facebook_id),
    facebook_name = wf:session(facebook_name),
    facebook_link = wf:session(facebook_link),
    twitter_id = wf:session(twitter_id),
    twitter_name = wf:session(twitter_name),
    redirect_url = wf:session(redirect_url)
  }.

set_state(undefined) ->
  ok;
set_state(#session_state{
             user = User,
             email = Email,
             facebook_id = FacebookId,
             facebook_name = FacebookName,
             facebook_link = FacebookLink,
             twitter_id = TwitterId,
             twitter_name = TwitterName,
             redirect_url = RedirectUrl
            }) ->
  wf:user(User),
  wf:session(user_email, Email),
  wf:session(facebook_id, FacebookId),
  wf:session(facebook_name, FacebookName),
  wf:session(facebook_link, FacebookLink),
  wf:session(twitter_id, TwitterId),
  wf:session(twitter_name, TwitterName),
  wf:session(redirect_url, RedirectUrl).
