%% -*- mode: nitrogen -*-
-module (mt_index).

-export([
  main/0,
  title/0,
  header/0,
  body/0,
  event/1,
  author/0,
  url/0
]).

-include_lib("nitrogen_core/include/wf.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").

main() -> #template { file="./site/templates/metalkia/bare.html" }.

title() -> "Metalkia".


author() ->
  ?DBG("Author", []),
  "Metachord".

url() ->
  ?DBG("URL", []),
  "http://metalkia.com".


header() ->
  #container_16 { body=[
    #grid_8 { alpha=true, prefix=7, suffix=0, body=inner_header() }
  ]}.

body() ->
  #container_16 { body=[
    #grid_8 { alpha=true, prefix=0, suffix=0, body="" }
  ], omega = true}.

inner_header() ->
  IsTwSig = mt_twitter:is_signed_in(),
  IsFbSig = mt_facebook:is_signed_in(),
  #panel{body=[
    mt_facebook:login_panel(),
    mt_twitter:login_panel(),
    if
      IsTwSig orelse IsFbSig ->
        mt_logoff:logoff_panel();
      true ->
        []
    end
  ]}.

event("logoff") ->
  wf:redirect(mtc:get_env(url) ++ "/logoff").

