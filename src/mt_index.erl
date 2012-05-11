%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


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
  "Metachord".

header() ->
  "".

body() ->
  wf:redirect("/info").

event(_) ->
  ok.

