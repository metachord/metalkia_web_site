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

body() ->
  PathInfo = wf:path_info(),
  ?DBG("PathInfo: ~p", [PathInfo]),
  Streams = dict:fetch(streams, PathInfo),
  Posts =
  [mtc_entry:sget(mt_post, Key) ||
    Key <- lists:umerge(
             [lists:umerge(
                [mtc_entry:sget(tags, UserName, Tag) ||
                  Tag <- Tags]) ||
               #mt_stream{username = UserName, tags = Tags} <- Streams])],
  ?DBG("Result:~n~p", [Posts]),
  "".

event("logoff") ->
  wf:redirect(mtc:get_env(url) ++ "/logoff").

