%% -*- mode: nitrogen -*-
-module (mt_index).

-export([
  main/0,
  header/0,
  body/0,
  author/0,
  event/1
]).

-include_lib("nitrogen_core/include/wf.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_records.hrl").

main() -> #template { file="./site/templates/metalkia/bare.html" }.

author() ->
  ?DBG("Author", []),
  "Metachord".

header() ->
  "".

body() ->
  PathInfo = wf:path_info(),
  ?DBG("PathInfo: ~p", [PathInfo]),
  "".

event("logoff") ->
  wf:redirect(mtc:get_env(url) ++ "/logoff").

