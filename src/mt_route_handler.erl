% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mt_route_handler).
-behaviour (route_handler).
-include_lib ("nitrogen_core/include/wf.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").
-include_lib("metalkia_core/include/mt_records.hrl").
-export ([
  init/2,
  finish/2
]).

%% @doc

%% The static route handler simply directs all requests to the
%% provided page module. This is used by the nitrogen_webmachine along
%% with the webmachine dispatch table to send requests through
%% webmachine to a Nitrogen page.

init(PageModule, State) ->
  RequestBridge = wf_context:request_bridge(),
  if PageModule =:= [] ->
      Path = RequestBridge:path(),

      PathInfo = wf_context:path_info(),

      {Module, PathInfo1} = route(Path, PathInfo),
      {Module1, PathInfo2} = check_for_404(Module, PathInfo1, Path),

      %% FIXME
      %% case Module1 of
      %%   file_not_found_page ->
      %%     wf_context:page_module(PageModule);
      %%   _ ->
      %%     wf_context:page_module(Module1)
      %% end,
      wf_context:page_module(Module1),
      wf_context:path_info(PathInfo2);
     true ->
      wf_context:page_module(PageModule),
      wf_context:event_module(PageModule)
  end,
  {ok, State}.

finish(_Config, State) ->
  {ok, State}.

%%
%% route("/", PathInfo) ->
%%   {mt_index, PathInfo};

route(Path, PathInfo) ->
  IsStatic = (filename:extension(Path) /= []),
  if
    IsStatic ->
      {static_file, Path};
    true ->
      case user_blog(PathInfo) of
        {undefined, Blog} ->
          ?ERR("Blog ~p not registered", [Blog]),
          {mt_index, PathInfo};
        {UserName, BlogName, Streams, NewPathInfo} ->
          route_blog(UserName, BlogName, Streams, Path, NewPathInfo);
        _ ->
          {mt_index, PathInfo}
      end
  end.

route_blog(UserName, _BlogName, Streams, Path, PathInfo) ->
  Profile = mtc_entry:sget(mt_person, ?a2b(UserName)),
  PathInfo1 =
    lists:foldl(
      fun({Key, Value}, PI) ->
          if Value =/= undefined ->
              dict:store(Key, Value, PI);
             true ->
              PI
          end
      end, PathInfo, [
                      {username, UserName},
                      {profile, Profile},
                      {streams, Streams}
                     ]),
  case Path of
    "/profile" ->
      {mt_profile, PathInfo1};
    "/logoff" ->
      {mt_logoff, PathInfo1};
    "/facebook" ->
      {mt_facebook, PathInfo1};
    "/twitter" ->
      {mt_twitter, PathInfo1};
    "/post-add" ->
      PathInfo2 = dict:store(post_id, post_add, PathInfo1),
      {mt_post, PathInfo2};
    "/post/" ++ _PostArgs ->
      {mt_post, PathInfo1};
    "/" ->
      case Streams of
        [] ->
          {mt_index, PathInfo1};
        _ ->
          {mt_post, PathInfo1}
      end;
    _ ->
      {mt_index, PathInfo1}
  end.

-spec user_blog(dict()) -> {binary() | undefined, binary()}.
user_blog(PathInfo) ->
  HostTokensRev = dict:fetch(host_tokens, PathInfo),
  {match, [SiteUrl]} = re:run(mtc:get_env(url), "^(https?://)?(?<HOST>[^/:]+)(:.*)?$", [{capture, ['HOST'], list}]),
  [Tld, SiteName] = lists:reverse(string:tokens(SiteUrl, ".")),
  case HostTokensRev of
    [Tld, SiteName | Rest] ->
      %% Request to Metalkia
      BlogName = default,
      BlogTitle = "Metalkia (beta)",
      NewPathInfo =
        lists:foldl(
          fun({Key, Value}, PI) ->
              dict:store(Key, Value, PI)
          end, PathInfo, [
                          {blog, BlogName},
                          {blog_title, BlogTitle}
                         ]),
      {UserName, Streams} =
      case Rest of
        [UN] -> {UN, [#mt_stream{username = ?a2b(UN), tags = []}]};
        [] -> {none, []};
        _ -> {undefined, []}
      end,
      {UserName, BlogName, Streams, NewPathInfo};
    _ ->
      %% Search blog for this CNAME
      case mtc_entry:sget(mt_cname, list_to_binary(string:join(lists:reverse(HostTokensRev), "."))) of
        #mt_cname{cname = CName, title = BlogTitle, owner = Owner, streams = Streams} ->
          BlogName = ?a2l(CName),
          NewPathInfo =
            lists:foldl(
              fun({Key, Value}, PI) ->
                  dict:store(Key, Value, PI)
              end, PathInfo, [
                              {blog, BlogName},
                              {blog_title, ?a2l(BlogTitle)}
                             ]),
          {Owner, BlogName, Streams, NewPathInfo};
        _ ->
          {undefined, string:join(lists:reverse(HostTokensRev), ".")}
      end
  end.


check_for_404(static_file, _PathInfo, Path) ->
  {static_file, Path};

check_for_404(Module, PathInfo, Path) ->
    % Make sure the requested module is loaded. If it
    % is not, then try to load the web_404 page. If that
    % is not available, then default to the 'file_not_found_page' module.
    case code:ensure_loaded(Module) of
        {module, Module} -> {Module, PathInfo};
        _ ->
            case code:ensure_loaded(web_404) of
                {module, web_404} -> {web_404, Path};
                _ -> {file_not_found_page, Path}
            end
    end.
