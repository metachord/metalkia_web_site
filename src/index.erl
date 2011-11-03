%% -*- mode: nitrogen -*-
-module (index).

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
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() ->
  #panel{body=[
    #panel{body = #template{file = "./site/templates/metalkia/facebook_service.html"}}
  ]}.

event("click-" ++ Id) ->
  Level = wf:q("level-"++Id),
  Text = wf:q("textarea-"++Id),
  ?PRINT([{cid, Id}, {level, Level}, {text, Text}]),
  {_, _, A} = now(),
  NewId = Id ++ "-" ++ integer_to_list(A),
  ?PRINT(NewId),
  wf:insert_bottom("pan-"++Id, "dummy-pan"),
  wf:replace("comment-items-" ++ Id, "dummy-item").

