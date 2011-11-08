% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (mt_route_handler).
-behaviour (route_handler).
-include_lib ("nitrogen_core/include/wf.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
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
  ?DBG("PageModule: ~p", [PageModule]),
  if PageModule =:= [] ->
      Path = RequestBridge:path(),

      {Module, PathInfo} = route(Path),
      {Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),

      %% FIXME
      case Module1 of
        file_not_found_page ->
          wf_context:page_module(PageModule);
        _ ->
          wf_context:page_module(Module1)
      end,
      ?DBG("Set PathInfo: ~p", [PathInfo1]),
      wf_context:path_info(PathInfo1);
     true ->
      wf_context:page_module(PageModule),
      wf_context:event_module(PageModule)
  end,
  {ok, State}.

finish(_Config, State) ->
  {ok, State}.


%%
route("/") ->
  {mt_index, []};

route(Path) ->
  ["/" | Resource] = filename:split(Path),
  case Resource of
    [UserName, "profile"] ->
      {mt_profile, [{username, UserName}]};
    _ ->
      {mt_index, []}
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
