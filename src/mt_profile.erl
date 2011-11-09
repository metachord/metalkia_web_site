%% -*- mode: nitrogen -*-
-module (mt_profile).

-export([
  main/0,
  title/0,
  body/0,
  author/0,
  url/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include("mtws.hrl").


main() -> #template { file="./site/templates/metalkia/bare.html" }.

title() -> "Add new post".

author() ->
  ?DBG("Author", []),
  "Zert".

url() ->
  ?DBG("URL", []),
  "http://metalkia.com".

body() ->
  PathInfo = wf_context:path_info(),
  ?DBG("PathInfo: ~p", [PathInfo]),
  ?DBG("User: ~p", [wf:user()]),
  Email = mtws_common:get_email(),
  ?DBG("Email: ~p", [Email]),
  if
    Email =:= undefined ->
      wf:redirect(mtws_common:blog_link());
    true ->
      [
        #gravatar{email = mtws_common:get_email(), rating = "g"},
        #p{},
        #label{text = "Name"},
        #inplace_textbox{text = mtws_common:username()}
      ]
  end.
