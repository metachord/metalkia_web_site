-module(mt_logoff).

-export([
  main/0,
  logoff_panel/0
]).

%% For template
-export([
  button_link/0,
  button_text/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

main() ->
  wf:logout(),
  RedirectUrl =
  case wf:q(redirect_url) of
    undefined ->
      mtc:get_env(url);
    Url -> Url
  end,
  case wf:q(logoff_domain) of
    undefined ->
      wf:redirect(RedirectUrl);
    _ ->
      wf:redirect(mtc:get_env(url) ++ "/logoff" ++
        "?redirect_url=" ++ mtc_util:uri_encode(RedirectUrl))
  end.

logoff_panel() ->
  #panel{body = #template{file = "./site/templates/metalkia/logoff_service.html"}}.


button_link() ->
  mtws_common:base_uri() ++ "/logoff"
  "?redirect_url=" ++ mtc_util:uri_encode(mtws_common:url()) ++
  "&logoff_domain=" ++ mtc_util:uri_encode(mtws_common:base_uri()).

button_text() ->
  "Sign out".
