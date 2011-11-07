%% -*- mode: nitrogen -*-
-module (mt_index).

-export([
  main/0,
  title/0,
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


body() ->
  ?DBG("Cookies:~n~p", [wf_context:cookies()]),
  #container_12 { body=[
    #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
  ]}.

inner_body() ->
  #panel{body=[
    #panel{body = #template{file = "./site/templates/metalkia/facebook_service.html"}}
    %%#button{id=submit, text="LogOff",postback="logoff"}
  ]}.

event("logoff") ->
  wf:redirect("http://metalkia.com/logoff").
