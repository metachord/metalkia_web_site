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
  wf:redirect(mtc:get_env(url)).

logoff_panel() ->
  #panel{body = #template{file = "./site/templates/metalkia/logoff_service.html"}}.


button_link() ->
  mtws_common:base_uri() ++ "/logoff".

button_text() ->
  "Sign out".
